{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Internal
  ( Coercing(..), Request(..), R(..), wrap, unwrap, Response
  , Auth(..), Format(..)
  , render
  ) where

import Control.Applicative (Applicative(..))
import Data.Monoid

import           Data.Aeson (Value, decode)
import           Network.URI (escapeURIChar, isUnreserved)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


class Coercing t where
  coerce ∷ t (a ∷ Auth) b → t c d


-- | Lastfm API request data type
--
-- @a@ is authentication method
--
-- @f@ is response format
data R (f ∷ Format) (a ∷ Auth) t = R
  { host ∷ Text
  , method ∷ Strict.ByteString
  , query ∷ Map Text Text
  , parse ∷ Lazy.ByteString → Response f
  }

-- | Response format: either JSON or XML
data Format = JSON | XML

-- | Authentication method
data Auth =
    Send -- ^ Public API. Doesn't require anything special besides API key
  | Sign -- ^ Private API. Requires Session key and Secret as well as API key

instance Coercing (R f) where
  coerce R { host = h, method = m, query = q, parse = p } = R { host = h, method = m, query = q, parse = p }
  {-# INLINE coerce #-}

instance Default (R JSON a t) where
  def = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = M.fromList [("format", "json")]
    , parse = decode
    }
  {-# INLINE def #-}

instance Default (R XML a t) where
  def = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = mempty
    , parse = id
    }
  {-# INLINE def #-}


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


type family Response (f ∷ Format)
type instance Response JSON = Maybe Value
type instance Response XML = Lazy.ByteString


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
unwrap ∷ Request f a t → (R f a t → R f a t)
unwrap = appEndo . getDual . unRequest
{-# INLINE unwrap #-}
