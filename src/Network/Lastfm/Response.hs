{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request sending and Response parsing
module Network.Lastfm.Response
  ( -- * Sign Request
    -- $sign
    Session, Secret, sign
    -- * Get Response
  , lastfm
  ) where

import Control.Applicative
import Data.Monoid
import Unsafe.Coerce (unsafeCoerce)

import           Control.Lens
import qualified Data.ByteString.Lazy as B
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


-- | Session key
type Session = Text

-- | Application secret
type Secret = Text

-- | Sign 'Request' with 'Secret'
sign ∷ Session → Secret → Request RequireSign f → Request Ready f
sign sk s = approve . (<> signature) . (<> wrap (query %~ M.insert "sk" sk))
 where
  signature = wrap $ \r → query %~ M.insert "api_sig" (signer (M.delete "format" (_query r))) $ r

  signer = T.pack . show . md5 . T.encodeUtf8 . (<> s) . mconcat . map (uncurry (<>)) . M.toList


-- | Send Request and parse Response
lastfm ∷ Default (R Ready f) ⇒ Request Ready f → IO (Response f)
lastfm req = do
  let t = unwrap req def
  parse t <$> C.withManager (\m → C.parseUrl (render t) >>= \url →
    C.responseBody <$> C.httpLbs (url { C.method = B.toStrict . T.encodeUtf8 $ _method t }) m)


approve ∷ Request RequireSign f → Request Ready f
approve = unsafeCoerce
