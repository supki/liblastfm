module Network.Lastfm.Core
  ( Response
  , withSecret
  , firstInnerTagContent, allInnerTagsContent, getAllInnerTags
  , callAPI, callAPI_
  ) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.IORef
import Data.List (sortBy)
import Data.URLEncoded (urlEncode, export)
import Network.Curl hiding (Content)
import System.IO.Unsafe (unsafePerformIO)
import Text.XML.Light

import qualified Data.ByteString.Lazy.Char8 as BS

type Key = String
type Value = String
type Secret = String
type Sign = String

newtype Response = Response {unwrap :: [Element]}

secret :: IORef Secret
secret = unsafePerformIO $ newIORef ""

withSecret :: Secret -> IO a -> IO a
withSecret s f = writeIORef secret s >> f

callAPI :: [(Key, Value)] -> IO Response
callAPI as = withCurlDo $ do
               s <- readIORef secret
               handle <- initialize
               response <- liftM (Response . onlyElems . parseXML . decodeString . respBody)
                            (do_curl_ handle
                                      "http://ws.audioscrobbler.com/2.0/?"
                                      [ CurlPostFields . map (export . urlEncode) $ (("api_sig", sign s) : as) ]
                                      :: IO CurlResponse)
               reset handle
               case firstInnerTagContent "error" response of
                 Just m  -> error m
                 Nothing -> return response
  where sign :: Secret -> Sign
        -- ^ Each API call (a little exception for getToken) should be signed.
        -- Algorithm description can be found at http://www.lastfm.ru/api/authspec Section 8
        sign s = show . md5 . BS.pack . (++ s) . concatMap (uncurry (++)) . sortBy (compare `on` fst) $ as

callAPI_ :: [(Key, Value)] -> IO ()
callAPI_ as = callAPI as >> return ()

firstInnerTagContent :: String -> Response -> Maybe String
firstInnerTagContent tag response = liftM strContent (maybeHead . unwrap . getAllInnerTags tag $ response)
  where maybeHead :: [a] -> Maybe a
        maybeHead [] = Nothing
        maybeHead xs = Just $ head xs

allInnerTagsContent :: String -> Response -> [String]
allInnerTagsContent tag = map strContent <$> unwrap . getAllInnerTags tag

getAllInnerTags ::  String -> Response -> Response
getAllInnerTags tag = Response . concatMap (findElements . unqual $ tag) . unwrap
