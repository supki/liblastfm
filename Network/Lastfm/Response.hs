{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
-- | Response module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.Response
  ( Lastfm, Response, LastfmError(WrapperCallError), dispatch
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
  = DoesntExist
  | InvalidService
  | InvalidMethod
  | AuthenticationFailed
  | InvalidFormat
  | InvalidParameters
  | InvalidResource
  | OperationFailed
  | InvalidSessionKey
  | InvalidAPIKey
  | ServiceOffline
  | SubscribersOnly
  | InvalidMethodSignature
  | TokenHasNotAuthorized
  | NotForStreaming
  | TemporaryUnavailable
  | LoginRequired
  | TrialExpired
  | DoesntExistAgain
  | NotEnoughContent
  | NotEnoughMembers
  | NotEnoughFans
  | NotEnoughNeighbours
  | NoPeakRadio
  | RadioNotFound
  | SuspendedAPIKey
  | Deprecated
  | RateLimitExceeded
    deriving (Enum)

instance Show APIError where
  show DoesntExist = "DoesntExist: This error does not exist"
  show InvalidService = "InvalidService: This service does not exist"
  show InvalidMethod = "InvalidMethod: No method with that name in this package"
  show AuthenticationFailed = "AuthenticationFailed: You do not have permissions to access the service"
  show InvalidFormat = "InvalidFormat: This service doesn't exist in that format"
  show InvalidParameters = "InvalidParameters: Your request is missing a required parameter"
  show InvalidResource = "InvalidResource: Invalid resource specified"
  show OperationFailed = "OperationFailed: Something else went wrong"
  show InvalidSessionKey = "InvalidSessionKey: Please re-authenticate"
  show InvalidAPIKey = "InvalidAPIKey: You must be granted a valid key by last.fm"
  show ServiceOffline = "ServiceOffline: This service is temporarily offline. Try again later."
  show SubscribersOnly  = "SubscribersOnly : This station is only available to paid last.fm subscribers"
  show InvalidMethodSignature = "InvalidMethodSignature: Invalid method signature supplied"
  show TokenHasNotAuthorized = "TokenHasNotAuthorized: This token has not been authorized"
  show NotForStreaming = "NotForStreaming: This item is not available for streaming."
  show TemporaryUnavailable = "TemporaryUnavailable: The service is temporarily unavailable, please try again."
  show LoginRequired = "LoginRequired: Login: User requires to be logged in"
  show TrialExpired = "TrialExpired: This user has no free radio plays left. Subscription required."
  show DoesntExistAgain = "DoesntExistAgain: This error does not exist"
  show NotEnoughContent = "NotEnoughContent: There is not enough content to play this station"
  show NotEnoughMembers = "NotEnoughMembers: This group does not have enough members for radio"
  show NotEnoughFans = "NotEnoughFans: This artist does not have enough fans for for radio"
  show NotEnoughNeighbours = "NotEnoughNeighbours: There are not enough neighbours for radio"
  show NoPeakRadio = "NoPeakRadio: This user is not allowed to listen to radio during peak usage"
  show RadioNotFound = "RadioNotFound: Radio station not found"
  show SuspendedAPIKey = "SuspendedAPIKey: Access for your account has been suspended, please contact Last.fm"
  show Deprecated = "Deprecated: This type of request is no longer supported"
  show RateLimitExceeded = "RateLimitExceeded: Your IP has made too many requests in a short period"

-- Various Lastfm errors.
data LastfmError
  = LastfmAPIError APIError
  | WrapperCallError Method Message
    deriving (Show, Typeable)

instance Exception LastfmError

-- | Type synonym for Lastfm response or error.
type Lastfm a = IO (Either LastfmError a)
-- | Type synonym for Lastfm response
type Response = String
type Key = String
type Value = String
type Secret = String
type Sign = String
type Method = String
type Message = String

secret :: IORef Secret
secret = unsafePerformIO $ newIORef ""

-- | All authentication requiring functions should be wrapped into 'withSecret'
withSecret :: Secret -> IO a -> IO a
withSecret s f = writeIORef secret s >> f

url :: String
url = "http://ws.audioscrobbler.com/2.0/?"

-- | Low level function. Captures all exceptions and transform them into Either Error type.
dispatch :: IO a -> Lastfm a
dispatch f = handle (\(e :: LastfmError) -> return (Left e)) (liftM Right f)

-- | Low level function. Sends POST query to Lastfm API.
callAPI :: Method -> [(Key, Value)] -> IO Response
callAPI m xs = withCurlDo $ do
                 s <- readIORef secret
                 let ys = ("method", m) : filter (not . null . snd) xs
                 let zs = if not $ null s then ("api_sig", sign s ys) : ys else ys
                 response <- decodeString . respBody <$> (curlGetResponse_ url
                                                           [ CurlPostFields . map (export . urlEncode) $ zs
                                                           , CurlFailOnError False
                                                           , CurlUserAgent "Mozilla/5.0 (X11; Linux x86_64; rv:10.0) Gecko/20100101 Firefox/10.0 Iceweasel/10.0"
                                                           ]
                                                           :: IO CurlResponse)
                 case isError response of
                   Just n  -> throw $ LastfmAPIError (toEnum $ n - 1)
                   Nothing -> return response

  where -- | Some API calls should be signed. (http://www.lastfm.ru/api/authspec Section 8)
        sign :: Secret -> [(Key, Value)] -> Sign
        sign s = show . md5 . BS.pack . (++ s) . concatMap (uncurry (++)) . sortBy (compare `on` fst)

        isError :: String -> Maybe Int
        isError response = do xml <- parseXMLDoc response
                              read <$> (findAttr (unqual "code") <=< findChild (unqual "error")) xml
