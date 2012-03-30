{-# LANGUAGE ScopedTypeVariables #-}
-- | Response module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm
  ( Lastfm, Response, dispatch
  , callAPI, callAPIsigned
  , module Network.Lastfm.Types
  ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Exception (handle, throw)
import Control.Monad ((<=<))
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List (sortBy)
import Data.URLEncoded (urlEncode, export)
import Network.Curl hiding (Content)
import Network.Lastfm.Types
import Text.XML.Light

import qualified Data.ByteString.Lazy.Char8 as BS

-- | Type synonym for Lastfm response or error.
type Lastfm a = IO (Either LastfmError a)
-- | Type synonym for Lastfm response
type Response = String

-- | Low level function. Captures all exceptions and transform them into Either Error type.
dispatch :: IO a -> Lastfm a
dispatch f = handle (\(e :: LastfmError) -> return (Left e)) (Right <$> f)

-- | Low level function. Sends POST query to Lastfm API.
callAPI :: [(String, String)] -> IO Response
callAPI xs = query ys
  where ys = map (second encodeString) . filter (not . null . snd) $ xs

callAPIsigned :: Secret -> [(String, String)] -> IO Response
callAPIsigned (Secret s) xs = query zs
  where ys = map (second encodeString) . filter (not . null . snd) $ xs
        zs = ("api_sig", sign ys) : ys

        sign :: [(String, String)] -> String
        sign = show . md5 . BS.pack . (++ s) . concatMap (uncurry (++)) . sortBy (compare `on` fst)

query :: [(String, String)] -> IO Response
query xs = withCurlDo $ do
  response <- respBody <$> (curlGetResponse_ "http://ws.audioscrobbler.com/2.0/?"
                             [ CurlPostFields . map (export . urlEncode) $ xs
                             , CurlFailOnError False
                             , CurlUserAgent "Mozilla/5.0 (X11; Linux x86_64; rv:10.0) Gecko/20100101 Firefox/10.0 Iceweasel/10.0"
                             ]
                             :: IO CurlResponse)
  maybe (return response) (\n -> throw $ LastfmAPIError (toEnum $ n - 1)) (isError response)
  where isError :: String -> Maybe Int
        isError response = do xml <- parseXMLDoc response
                              read <$> (findAttr (unqual "code") <=< findChild (unqual "error")) xml
