{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Response where

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

import Network.Lastfm.Request


type Session = Text
type Secret = Text


sign ∷ Session → Secret → Request RequireSign f → Request Ready f
sign sk s = (<> signature) . (<> wrap (__query %~ M.insert "session_key" sk)) . unsafeCoerce
 where
  signature = wrap $ \r → __query %~ M.insert "api_sig" (signer (_query r)) $ r

  signer = T.pack . show . md5 . T.encodeUtf8 . (<> s) . mconcat . map (uncurry (<>)) . M.toList


-- | Send Request and parse Response
lastfm ∷ Default (R Ready f) ⇒ Request Ready f → IO (Response f)
lastfm req = do
  let t = unwrap req def
  _parse t <$> C.withManager (\m → C.parseUrl (render t) >>= \url →
    C.responseBody <$> C.httpLbs (url { C.method = B.toStrict . T.encodeUtf8 $ _method t }) m)


render ∷ R a f → String
render R { _host = h, _query = q } =
  T.unpack $ mconcat [h, "?", argie q]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m → T.concat [k, "=", v] : m) mempty
