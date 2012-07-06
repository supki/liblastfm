{-# LANGUAGE ScopedTypeVariables #-}
-- | Response module
{-# OPTIONS_HADDOCK hide #-}
module Network.Lastfm.Internal
  ( ResponseType(..)
  , callAPI, callAPIsigned, xml, json, (#)
  , module Network.Lastfm
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&), second)
import Data.Function (on)
import Data.List (sortBy)

import Codec.Binary.UTF8.String (encodeString)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Digest.Pure.MD5 (md5)
import Data.URLEncoded (urlEncode, export)
import Network.Curl

import Network.Lastfm
import Network.Lastfm.Error
import Network.Lastfm.TH


-- Send POST query to Lastfm API
callAPI ∷ ResponseType → [(String, String)] → Lastfm Response
callAPI t = query (parseError t) . insertType t . map (second encodeString)


-- Send signed POST query to Lastfm API
callAPIsigned ∷ ResponseType → Secret → [(String, String)] → Lastfm Response
callAPIsigned t (Secret s) xs = query (parseError t) zs
 where
  ys = map (second encodeString) . filter (not . null . snd) $ xs
  zs = insertType t $ ("api_sig", sign ys) : ys

  sign ∷ [(String, String)] → String
  sign = show . md5 . BS.pack . (++ s) . concatMap (uncurry (++)) . sortBy (compare `on` fst)


-- Insert desired response type into query
insertType ∷ ResponseType → [(String, String)] → [(String, String)]
insertType XML = id
insertType JSON = (("format", "json") :)


-- Try to find error message in Lastfm response
parseError ∷ ResponseType → ByteString → Maybe LastfmError
parseError XML = xmlError
parseError JSON = jsonError


query ∷ (ByteString → Maybe LastfmError) → [(String, String)] → IO (Either LastfmError Response)
query γ xs = do
  (status, body) ← (respCurlCode &&& respBody) <$> curlResponse xs
  return $ case status of
    CurlOK → case γ body of
               Nothing → Right body
               Just n → Left n
    s → Left $ CurlError s


curlResponse ∷ [(String, String)] → IO (CurlResponse_ [(String, String)] ByteString)
curlResponse xs = withCurlDo $ curlGetResponse_ "http://ws.audioscrobbler.com/2.0/?"
  [ CurlPostFields . map (export . urlEncode) $ xs
  , CurlFailOnError False
  , CurlUserAgent "Mozilla/5.0 (X11; Linux x86_64; rv:10.0) Gecko/20100101 Firefox/10.0 Iceweasel/10.0"
  , CurlConnectTimeout 10
  ]


-- Construct query parameter
(#) ∷ Argument a ⇒ a → (String, String)
(#) x = (key x, value x)
