{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances,
 ScopedTypeVariables, DeriveDataTypeable #-}
module Network.Lastfm.Core
  ( Lastfm, Response, LastfmError(..), dispatch
  , withSecret
  , firstInnerTagContent, allInnerTagsContent, getAllInnerTags
  , LastfmValue(..), (?<), callAPI, callAPI_
  ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Applicative ((<$>))
import Control.Exception (Exception, handle, throw)
import Control.Monad (liftM)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.IORef
import Data.List (intercalate, sortBy)
import Data.Typeable (Typeable)
import Data.URLEncoded (urlEncode, export)
import Network.Curl hiding (Content)
import System.IO.Unsafe (unsafePerformIO)

import Text.XML.Light

import qualified Data.ByteString.Lazy.Char8 as BS

data LastfmError
  = DoesntExist -- ^ This error does not exist
  | InvalidService -- ^ This service does not exist
  | InvalidMethod -- ^ No method with that name in this package
  | AuthenticationFailed -- ^ You do not have permissions to access the service
  | InvalidFormat -- ^ This service doesn't exist in that format
  | InvalidParameters -- ^ Your request is missing a required parameter
  | InvalidResource -- ^ Invalid resource specified
  | OperationFailed -- ^ Something else went wrong
  | InvalidSessionKey -- ^ Please re-authenticate
  | InvalidAPIKey -- ^ You must be granted a valid key by last.fm
  | ServiceOffline -- ^ This service is temporarily offline. Try again later.
  | SubscribersOnly  -- ^ This station is only available to paid last.fm subscribers
  | InvalidMethodSignature -- ^ Invalid method signature supplied
  | TokenHasNotAuthorized -- ^ This token has not been authorized
  | NotForStreaming -- ^ This item is not available for streaming.
  | TemporaryUnavailable -- ^ The service is temporarily unavailable, please try again.
  | LoginRequired -- ^ Login: User requires to be logged in
  | TrialExpired -- ^ This user has no free radio plays left. Subscription required.
  | DoesntExistAgain -- ^ This error does not exist
  | NotEnoughContent -- ^ There is not enough content to play this station
  | NotEnoughMembers -- ^ This group does not have enough members for radio
  | NotEnoughFans -- ^ This artist does not have enough fans for for radio
  | NotEnoughNeighbours -- ^ There are not enough neighbours for radio
  | NoPeakRadio -- ^ This user is not allowed to listen to radio during peak usage
  | RadioNotFound -- ^ Radio station not found
  | SuspendedAPIKey -- ^ Access for your account has been suspended, please contact Last.fm
  | Deprecated -- ^ This type of request is no longer supported
  | RateLimitExceeded -- ^ Your IP has made too many requests in a short period
  | WrapperCallError Method Message
  deriving (Show, Typeable)

instance Exception LastfmError

dispatch :: IO a -> Lastfm a
dispatch f = handle (\(e :: LastfmError) -> return (Left e)) (liftM Right f)

type Lastfm a = IO (Either LastfmError a)
type Key = String
type Value = String
type Secret = String
type Sign = String
type Method = String
type Message = String

newtype Response = Response {unwrap :: [Element]}

secret :: IORef Secret
secret = unsafePerformIO $ newIORef ""

withSecret :: Secret -> IO a -> IO a
withSecret s f = writeIORef secret s >> f

class LastfmValue a where
  unpack :: a -> String

instance LastfmValue Bool where
  unpack True = "1"
  unpack False = "0"
instance LastfmValue Int where
  unpack = show
instance LastfmValue String where
  unpack = id
instance LastfmValue a => LastfmValue [a] where
  unpack = intercalate "," . map unpack
instance LastfmValue a => LastfmValue (Maybe a) where
  unpack (Just a) = unpack a
  unpack Nothing  = ""

(?<) :: LastfmValue a => String -> a -> (String, String)
a ?< b = (a, unpack b)

callAPI :: Method -> [(Key, Value)] -> IO Response
callAPI m as = withCurlDo $ do
                 s <- readIORef secret
                 curl <- initialize
                 response <- liftM (Response . onlyElems . parseXML . decodeString . respBody)
                              (do_curl_ curl
                                        "http://ws.audioscrobbler.com/2.0/?"
                                        [ CurlPostFields . map (export . urlEncode) $ (("api_sig", sign s) : bs) ]
                                        :: IO CurlResponse)
                 reset curl
                 case firstInnerTagContent "error" response of
                   Just _  -> throw DoesntExist
                   Nothing -> return response
  where bs :: [(Key, Value)]
        bs = filter (not . null . snd) $ ("method", m) : as
        sign :: Secret -> Sign
        -- ^ Each API call (a little exception for getToken) should be signed.
        -- Algorithm description can be found at http://www.lastfm.ru/api/authspec Section 8
        sign s = show . md5 . BS.pack . (++ s) . concatMap (uncurry (++)) . sortBy (compare `on` fst) $ bs

callAPI_ :: Method -> [(Key, Value)] -> IO ()
callAPI_ m as = callAPI m as >> return ()

firstInnerTagContent :: String -> Response -> Maybe String
firstInnerTagContent tag response = liftM strContent (maybeHead . unwrap . getAllInnerTags tag $ response)
  where maybeHead :: [a] -> Maybe a
        maybeHead [] = Nothing
        maybeHead xs = Just $ head xs

allInnerTagsContent :: String -> Response -> [String]
allInnerTagsContent tag = map strContent <$> unwrap . getAllInnerTags tag

getAllInnerTags ::  String -> Response -> Response
getAllInnerTags tag = Response . concatMap (findElements . unqual $ tag) . unwrap
