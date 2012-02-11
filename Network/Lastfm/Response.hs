{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Network.Lastfm.Response
  ( Lastfm, Response, LastfmError(..), dispatch
  , withSecret
  , callAPI
  ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Applicative ((<$>))
import Control.Exception (Exception, handle, throw)
import Control.Monad ((<=<), liftM)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.IORef
import Data.List (sortBy)
import Data.Typeable (Typeable)
import Data.URLEncoded (urlEncode, export)
import Network.Curl hiding (Content)
import System.IO.Unsafe (unsafePerformIO)
import Text.XML.Light

import qualified Data.ByteString.Lazy.Char8 as BS

data APIError
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
    deriving (Show, Enum)

data LastfmError
  = LastfmAPIError APIError
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

type Response = String

secret :: IORef Secret
secret = unsafePerformIO $ newIORef ""

withSecret :: Secret -> IO a -> IO a
withSecret s f = writeIORef secret s >> f

url :: String
url = "http://ws.audioscrobbler.com/2.0/?"

callAPI :: Method -> [(Key, Value)] -> IO Response
callAPI m xs = withCurlDo $ do
                 s <- readIORef secret
                 let ys = ("method", m) : filter (not . null . snd) xs
                 let zs = if not $ null s then ("api_sig", sign s ys) : ys else ys
                 response <- decodeString . respBody <$> (curlGetResponse_ url
                                                           [ CurlPostFields . map (export . urlEncode) $ zs, CurlFailOnError False ]
                                                           :: IO CurlResponse)
                 print response
                 case isError response of
                   Just n  -> throw $ LastfmAPIError (toEnum $ n - 1)
                   Nothing -> return response

  where -- | Some API calls should be signed. (http://www.lastfm.ru/api/authspec Section 8)
        sign :: Secret -> [(Key, Value)] -> Sign
        sign s = show . md5 . BS.pack . (++ s) . concatMap (uncurry (++)) . sortBy (compare `on` fst)

        isError :: String -> Maybe Int
        isError response = do xml <- parseXMLDoc response
                              read <$> (findAttr (unqual "code") <=< findChild (unqual "error")) xml
