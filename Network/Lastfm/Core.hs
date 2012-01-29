module Network.Lastfm.Core
  ( callAPI, tagContent
-- , LastfmError
  ) where

import Control.Monad (join, liftM)
import Data.URLEncoded (urlEncode, export)
import Network.Curl hiding (Content)
import Text.XML.Light

{-
data LastfmError
  = InvalidService -- ^ This service does not exist
  | InvalidMethod -- ^ No method with that name in this package
  | AuthenticationFailed -- ^ You do not have permissions to access the service
  | InvalidFormat -- ^ This service doesn't exist in that format
  | InvalidParameters -- ^ Your request is missing a required parameter
  | InvalidResource -- ^ Invalid resource specified
  | OperationFailed -- ^ Something else went wrong
  | InvalidSessionKey -- ^ Please re-authenticate
  | InvalidAPIKey -- ^ You must be granted a valid key by last.fm
  | ServiceOffline -- ^ This service is temporarily offline. Try again later.
  | InvalidMethodSignature -- ^ Invalid method signature supplied
  | TokenHasNotAuthorized -- ^ This token has not been authorized
  | TokenHasExpired -- ^ This token has expired
  | TemporaryProcessingError -- ^ There was a temporary error processing your request. Please try again
  | SuspendedAPIKey -- ^ Access for your account has been suspended, please contact Last.fm
  | RateLimitExceeded -- ^ Your IP has made too many requests in a short period
  deriving Show
-}

callAPI :: [(String, String)] -> IO [Element]
callAPI as = withCurlDo $ do
               handle <- initialize
               response <- liftM (onlyElems . parseXML . respBody) (do_curl_ handle "http://ws.audioscrobbler.com/2.0/?"
                            [ CurlPostFields . map (export . urlEncode) $ as ]
                            :: IO CurlResponse)
               case checkError response of
                 Just e  -> error e
                 Nothing -> return response

checkError :: [Element] -> Maybe String
checkError elements = do element <- maybeHead . concatMap (findElements . unqual $ "lfm") $ elements
                         attribute <- findAttr (unqual "status") element
                         case attribute of
                           "failed" -> tagContent "error" elements
                           _        -> Nothing

tagContent :: String -> [Element] -> Maybe String
tagContent tag elements = do element <- maybeHead . concatMap (findElements . unqual $ tag) $ elements
                             return $ strContent element

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just $ head xs
