{-# LANGUAGE BangPatterns, ScopedTypeVariables, TemplateHaskell #-}
-- | Response module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm
  ( Lastfm, Response, ResponseType(..)
  , callAPI, callAPIsigned
  , xml, json
  , module Network.Lastfm.Types
  ) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Digest.Pure.MD5 (md5)
import Data.Function (on)
import Data.List (sortBy)
import Data.URLEncoded (urlEncode, export)
import Language.Haskell.TH
import Network.Curl
import Network.Lastfm.Error
import Network.Lastfm.Types
import qualified Data.ByteString.Lazy.Char8 as BS

-- | Type synonym for Lastfm response or error.
type Lastfm a = IO (Either LastfmError a)
-- | Type synonym for Lastfm response
type Response = ByteString
-- | Desired type of Lastfm response
data ResponseType = XML | JSON

-- | Low level function. Sends POST query to Lastfm API.
callAPI ∷ ResponseType → [(String, String)] → Lastfm Response
callAPI t = runErrorT . query . insertType t . map (second encodeString)

-- | Low level function. Sends signed POST query to Lastfm API.
callAPIsigned ∷ ResponseType → Secret → [(String, String)] → Lastfm Response
callAPIsigned t (Secret s) xs = runErrorT $ query zs
  where ys = map (second encodeString) . filter (not . null . snd) $ xs
        zs = insertType t $ ("api_sig", sign ys) : ys

        sign ∷ [(String, String)] → String
        sign = show . md5 . BS.pack . (++ s) . concatMap (uncurry (++)) . sortBy (compare `on` fst)

insertType ∷ ResponseType → [(String, String)] → [(String, String)]
insertType XML = id
insertType JSON = (("format", "json") :)

query ∷ [(String, String)] → ErrorT LastfmError IO Response
query xs = do
  !response ← liftIO $ withCurlDo $ respBody <$> (curlGetResponse_ "http://ws.audioscrobbler.com/2.0/?"
                             [ CurlPostFields . map (export . urlEncode) $ xs
                             , CurlFailOnError False
                             , CurlUserAgent "Mozilla/5.0 (X11; Linux x86_64; rv:10.0) Gecko/20100101 Firefox/10.0 Iceweasel/10.0"
                             ]
                             ∷ IO (CurlResponse_ [(String, String)] ByteString))
  maybe (return response) (throwError . LastfmAPIError . toEnum . subtract 1) (getError response)
  where getError ∷ ByteString → Maybe Int
        getError _ = Nothing

xml ∷ [String] → Q [Dec]
xml = mapM func
  where func xs = funD (mkName xs) [clause [] (normalB $ appE (varE (mkName ("API." ++ xs))) [e| XML |]) []]

json ∷ [String] → Q [Dec]
json = mapM func
  where func xs = funD (mkName xs) [clause [] (normalB $ appE (varE (mkName ("API." ++ xs))) [e| JSON |]) []]
