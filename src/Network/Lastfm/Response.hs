{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request sending and Response parsing
module Network.Lastfm.Response
  ( -- * Sign Request
    -- $sign
    Secret, sign
    -- * Get Response
  , Response, lastfm, lastfm', finalize
  ) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad
import Data.Monoid

import           Data.Aeson ((.:), Value, decode, parseJSON)
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import           Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C

import Network.Lastfm.Internal


-- $sign
--
-- Signing is important part of every
-- authentication requiring API request.
-- Basically, every such request is appended
-- with md5 footprint of its arguments as
-- described at <http://www.last.fm/api/authspec#8>


class Supported (f ∷ Format) where
  type Response f
  parse ∷ R f a t → Lazy.ByteString → C.ResponseHeaders → Response f
  base ∷ R f a t


instance Supported JSON where
  type Response JSON = Maybe Value
  parse _ b hs = do
    v ← decode b
    case parseMaybe ((.: "error") <=< parseJSON) v of
      Just (_ ∷ Int) →
        throw (C.StatusCodeException C.status400 (("Response", Strict.concat $ Lazy.toChunks b) : hs))
      _ → return v
  base = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = M.fromList [("format", "json")]
    }
  {-# INLINE base #-}

instance Supported XML where
  type Response XML = Lazy.ByteString
  parse _ b _ = b
  base = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = mempty
    }
  {-# INLINE base #-}


-- | Application secret
type Secret = Text


-- | Sign 'Request' with 'Secret'
sign ∷ Secret → Request f Sign Ready → Request f Send Ready
sign s = approve . (<* signature)
 where
  signature = wrap $ \r@R { query = q } →
    r { query = M.insert "api_sig" (signer (foldr M.delete q ["format", "callback"])) q }

  signer = T.pack . show . md5 . T.encodeUtf8 . M.foldrWithKey(\k v xs → k <> v <> xs) s


-- | Send Request and parse Response
lastfm ∷ Supported f ⇒ Request f Send Ready → IO (Response f)
lastfm = lastfm' . finalize


-- | Get R from Request
--
-- That's rarely needed unless you want low-level manipulation of requests
finalize ∷ Supported f ⇒ Request f Send Ready → R f Send Ready
finalize = ($ base) . unwrap


-- | Send R and parse Response
--
-- That's rarely needed unless you want low-level manipulation of requests
lastfm' ∷ Supported f ⇒ R f Send Ready → IO (Response f)
lastfm' request =
  C.withManager (\m → C.parseUrl (render request) >>= \url → do
    t ← C.httpLbs (url
          { C.method = method request
          , C.responseTimeout = Just 10000000 }
          ) m
    return $ parse request (C.responseBody t) (C.responseHeaders t))


approve ∷ Request f Sign Ready → Request f Send Ready
approve = coerce
{-# INLINE approve #-}
