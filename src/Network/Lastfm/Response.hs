{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request sending and Response parsing
module Network.Lastfm.Response
  ( -- * Sign Request
    -- $sign
    Secret, sign
    -- * Get Response
  , lastfm
  ) where

import Control.Applicative
import Data.Monoid
import Unsafe.Coerce (unsafeCoerce)

import           Data.Default (Default(..))
import           Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Network.HTTP.Conduit as C

import Network.Lastfm.Internal


-- $sign
--
-- Signing is important part of every
-- authentication requiring API request.
-- Basically, every such request is appended
-- with md5 footprint of its arguments as
-- described at <http://www.last.fm/api/authspec#8>


-- | Application secret
type Secret = Text


-- | Sign 'Request' with 'Secret'
sign ∷ Secret → Request RequireSign f → Request Ready f
sign s = approve . (<> signature)
 where
  signature = wrap $ \r@R { query = q } →
    r { query = M.insert "api_sig" (signer (M.delete "format" q)) q }

  signer = T.pack . show . md5 . T.encodeUtf8 . (<> s) . mconcat . map (uncurry (<>)) . M.toList


-- | Send Request and parse Response
lastfm ∷ Default (R Ready f) ⇒ Request Ready f → IO (Response f)
lastfm req = do
  let t = unwrap req def
  parse t <$> C.withManager (\m → C.parseUrl (render t) >>= \url →
    C.responseBody <$> C.httpLbs (url { C.method = method t }) m)


approve ∷ Request RequireSign f → Request Ready f
approve = unsafeCoerce
