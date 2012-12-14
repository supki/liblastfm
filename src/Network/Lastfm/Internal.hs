{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Internal
  ( Request, R(..), wrap, unwrap, add, Response
  , Auth(..), Format(..)
  , render
  , api, post, get, json, xml
  ) where

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
  coerce ∷ t a → t b


-- | Lastfm API request data type
--
-- @a@ is authentication method
--
-- @f@ is response format
data R (f ∷ Format) (a ∷ Auth) = R
  { host ∷ Text
  , method ∷ Strict.ByteString
  , query ∷ Map Text Text
  , parse ∷ Lazy.ByteString → Response f
  }

-- | Response format: either JSON or XML
data Format = JSON | XML

-- | Authentication method
data Auth =
    Ready       -- ^ Public API. Doesn't require anything special besides API key
  | RequireSign -- ^ Private API. Requires Session key and Secret as well as API key

instance Coercing (R f) where
  coerce R { host = h, method = m, query = q, parse = p } = R { host = h, method = m, query = q, parse = p }

instance Default (R JSON a) where
  def = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = M.fromList [("format", "json")]
    , parse = decode
    }
  {-# INLINE def #-}

instance Default (R XML a) where
  def = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = M.fromList [("format", "xml")]
    , parse = id
    }
  {-# INLINE def #-}


type Request (f ∷ Format) (a ∷ Auth) = Dual (Endo (R f a))


type family Response (f ∷ Format)
type instance Response JSON = Maybe Value
type instance Response XML = Lazy.ByteString


render ∷ R f a → String
render R { host = h, query = q } =
  T.unpack $ mconcat [h, "?", argie q]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m → T.concat [escape k, "=", escape v] : m) []

  escape = T.concatMap (T.pack . escapeURIChar isUnreserved)


-- | Wrapping to interesting 'Monoid' ('R' -> 'R') instance
wrap ∷ (R f a → R f a) → Request f a
wrap = Dual . Endo
{-# INLINE wrap #-}


-- | Unwrapping from interesting 'Monoid' ('R' -> 'R') instance
unwrap ∷ Request f a → (R f a → R f a)
unwrap = appEndo . getDual
{-# INLINE unwrap #-}


-- | Change request API method
--
-- Primarily used in API call wrappers, not intended for usage by library user
api ∷ Text → Request f a
api = add "method"
{-# INLINE api #-}


-- | Change html method to GET
--
-- Primarily used in API call wrappers, not intended for usage by library user
get ∷ Request f a
get = wrap $ \r -> r { method = "GET" }
{-# INLINE get #-}


-- | Change html method to POST
--
-- Primarily used in API call wrappers, not intended for usage by library user
post ∷ Request f a
post = wrap $ \r -> r { method = "POST" }
{-# INLINE post #-}


-- | Change API response format to JSON
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
json ∷ Request JSON a
json = wrap id
{-# INLINE json #-}


-- | Change API response format to XML
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
xml ∷ Request XML a
xml = wrap id
{-# INLINE xml #-}


add ∷ Text → Text → Request f a
add k v = wrap $ \r@R { query = q } -> r { query = M.insert k v q }
{-# INLINE add #-}
