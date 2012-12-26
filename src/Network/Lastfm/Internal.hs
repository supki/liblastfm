{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Internal
  ( Coercing(..), Supported(..), Request(..), R(..), wrap, unwrap
  , Auth(..), Format(..)
  , render
  ) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad
import Data.Monoid

import           Data.Aeson ((.:), Value, decode, parseJSON)
import           Data.Aeson.Types (parseMaybe)
import           Data.Serialize (Serialize(..))
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Network.URI (escapeURIChar, isUnreserved)
import           Network.HTTP.Conduit (HttpException(..))
import           Network.HTTP.Types (ResponseHeaders)
import           Network.HTTP.Types.Status (status400)


class Coercing t where
  coerce ∷ t (a ∷ Auth) b → t c d

class Supported (f ∷ Format) where
  type Response f
  parse ∷ R f a t → Lazy.ByteString → ResponseHeaders → Response f
  base ∷ R f a t


-- | Lastfm API request data type
--
-- @a@ is authentication method
--
-- @f@ is response format
data R (f ∷ Format) (a ∷ Auth) t = R
  { host ∷ Text
  , method ∷ Strict.ByteString
  , query ∷ Map Text Text
  }

-- | Response format: either JSON or XML
data Format = JSON | XML

-- | Authentication method
data Auth =
    Send -- ^ Public API. Doesn't require anything special besides API key
  | Sign -- ^ Private API. Requires Session key and Secret as well as API key

instance Coercing (R f) where
  coerce R { host = h, method = m, query = q } = R { host = h, method = m, query = q }
  {-# INLINE coerce #-}

instance Supported JSON where
  type Response JSON = Maybe Value
  parse _ b hs = do
    v ← decode b
    case parseMaybe ((.: "error") <=< parseJSON) v of
      Just (_ ∷ Int) →
        throw (StatusCodeException status400 (("Response", Strict.concat $ Lazy.toChunks b) : hs))
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


newtype Request f a t = Request { unRequest ∷ Dual (Endo (R f a t)) }

instance Coercing (Request f) where
  coerce q = wrap $ coerce . unwrap q . coerce
  {-# INLINE coerce #-}

instance Functor (Request f a) where
  fmap _ = coerce
  {-# INLINE fmap #-}

instance Applicative (Request f a) where
  pure _ = wrap id
  f <*> x = let Request g = coerce f
                Request y = coerce x
            in Request $ g <> y
  {-# INLINE (<*>) #-}


render ∷ R f a t → String
render R { host = h, query = q } =
  T.unpack $ mconcat [h, "?", argie q]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m → T.concat [escape k, "=", escape v] : m) []

  escape = T.concatMap (T.pack . escapeURIChar isUnreserved)


-- | Wrapping to interesting 'Monoid' ('R' -> 'R') instance
wrap ∷ (R f a t → R f a t) → Request f a t
wrap = Request . Dual . Endo
{-# INLINE wrap #-}


-- | Unwrapping from interesting 'Monoid' ('R' -> 'R') instance
unwrap ∷ Request f a t → R f a t → R f a t
unwrap = appEndo . getDual . unRequest
{-# INLINE unwrap #-}


-- Miscellaneous instances

instance Serialize (R f a t) where
  put r = do
    put $ T.encodeUtf8 (host r)
    put $ method r
    put $ mapmap T.encodeUtf8 T.encodeUtf8 (query r)
  get = do
    h ← T.decodeUtf8 <$> get
    m ← get
    q ← mapmap T.decodeUtf8 T.decodeUtf8 <$> get
    return R { host = h, method = m, query = q }

mapmap ∷ (Ord s, Ord t) ⇒ (s → t) → (a → b) → Map s a → Map t b
mapmap f g = M.mapKeys f . M.map g
