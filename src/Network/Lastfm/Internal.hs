{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


-- | Authentication method
data Auth =
    Ready       -- ^ Public API. Doesn't require anything special besides API key
  | RequireSign -- ^ Private API. Requires Session key and Secret as well as API key


-- | Response format: either JSON or XML
data Format = JSON | XML


type Request (a ∷ Auth) (f ∷ Format) = Dual (Endo (R a f))


type family Response (f ∷ Format)
type instance Response JSON = Maybe Value
type instance Response XML = Lazy.ByteString


-- | Lastfm API request data type
--
-- @a@ is authentication method
--
-- @f@ is response format
data R (a ∷ Auth) (f ∷ Format) = R
  { host ∷ Text
  , method ∷ Strict.ByteString
  , query ∷ Map Text Text
  , parse ∷ Lazy.ByteString → Response f
  }


instance Default (R a JSON) where
  def = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = M.fromList [("format", "json")]
    , parse = decode
    }
  {-# INLINE def #-}


instance Default (R a XML) where
  def = R
    { host = "https://ws.audioscrobbler.com/2.0/"
    , method = "GET"
    , query = M.fromList [("format", "xml")]
    , parse = id
    }
  {-# INLINE def #-}


render ∷ R a f → String
render R { host = h, query = q } =
  T.unpack $ mconcat [h, "?", argie q]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m → T.concat [k, "=", v] : m) mempty


-- | Wrapping to interesting 'Monoid' ('R' -> 'R') instance
wrap ∷ (R a f → R a f) → Request a f
wrap = Dual . Endo
{-# INLINE wrap #-}


-- | Unwrapping from interesting 'Monoid' ('R' -> 'R') instance
unwrap ∷ Request a f → (R a f → R a f)
unwrap = appEndo . getDual
{-# INLINE unwrap #-}


-- | Change request API method
--
-- Primarily used in API call wrappers, not intended for usage by library user
api ∷ Text → Request a f
api = add "method"
{-# INLINE api #-}


-- | Change html method to GET
--
-- Primarily used in API call wrappers, not intended for usage by library user
get ∷ Request a f
get = wrap $ \r -> r { method = "GET" }
{-# INLINE get #-}


-- | Change html method to POST
--
-- Primarily used in API call wrappers, not intended for usage by library user
post ∷ Request a f
post = wrap $ \r -> r { method = "POST" }
{-# INLINE post #-}


-- | Change API response format to JSON
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
json ∷ Request a JSON
json = wrap id
{-# INLINE json #-}


-- | Change API response format to XML
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
xml ∷ Request a XML
xml = wrap id
{-# INLINE xml #-}


add ∷ Text → Text → Request a f
add k v = wrap $ \r@R { query = q } -> r { query = M.insert k v q }
{-# INLINE add #-}
