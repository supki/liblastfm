{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Request
  ( -- * Request
    Request, R(..), wrap, unwrap, Response
  , __method, __query
    -- * Request parameters
  , Auth(..), Format(..)
    -- * Request major parameters
  , api, method, json, xml, apiKey
    -- * Request minor parameters
  , Artist, artist, Album, album, MBID, mbid
  , Country, country, Autocorrect, autocorrect
  , Language, language
  , Tag, tags, tag
  , Recipient, recipient, Username, username, User, user
  , Public, public, Message, message, Page, page, Limit, limit
  , type', value
  ) where

import Data.Monoid ((<>), Dual(..), Endo(..))

import           Control.Lens hiding (value)
import           Data.Aeson (Value, decode)
import           Data.ByteString.Lazy (ByteString)
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


type Request a f = Dual (Endo (R a f))


type family Response (f ∷ Format)
type instance Response JSON = Maybe Value
type instance Response XML = ByteString


-- | Lastfm API request data type
--
-- @a@ is authentication method
--
-- @f@ is response format
data R (a ∷ Auth) (f ∷ Format) = R
  { _host ∷ Text
  , _method ∷ Text
  , _query ∷ Map Text Text
  , _parse ∷ ByteString → Response f
  }


instance Default (R a JSON) where
  def = R
    { _host = "http://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query = M.fromList [("format", "json")]
    , _parse = decode
    }


instance Default (R a XML) where
  def = R
    { _host = "http://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query = M.fromList [("format", "xml")]
    , _parse = id
    }


makeLensesFor [("_method", "__method"), ("_query", "__query")] ''R


-- | Wrapping to interesting 'Monoid' ('R' -> 'R') instance
wrap ∷ (R a f → R a f) → Request a f
wrap = Dual . Endo


-- | Unwrapping from interesting 'Monoid' ('R' -> 'R') instance
unwrap ∷ Request a f → (R a f → R a f)
unwrap = appEndo . getDual


-- | Change request API method
--
-- Primarily used in API call wrappers, not intended for usage by library user
api ∷ Text → Request a f
api m = wrap $ __query %~ M.insert "method" m
{-# INLINE api #-}


-- | Change html method
--
-- Primarily used in API call wrappers, not intended for usage by library user
method ∷ Text → Request a f
method m = wrap $ __method .~ m
{-# INLINE method #-}


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


-- | Change request API key
--
-- Primarily used in API call wrappers, not intended for usage by library user
apiKey ∷ Text → Request a f
apiKey m = wrap $ __query %~ M.insert "api_key" m
{-# INLINE apiKey #-}


type Artist = Text


-- | Add artist parameter
artist ∷ Artist → Request a f
artist a = wrap $ __query %~ M.insert "artist" a
{-# INLINE artist #-}


type Album = Text


-- | Add artist parameter
album ∷ Album → Request a f
album a = wrap $ __query %~ M.insert "album" a
{-# INLINE album #-}


type MBID = Text


-- | Add MBID parameter
mbid ∷ MBID → Request a f
mbid a = wrap $ __query %~ M.insert "mbid" a
{-# INLINE mbid #-}


type Country = Text


-- | Add country parameter
country ∷ Country → Request a f
country c = wrap $ __query %~ M.insert "country" c
{-# INLINE country #-}


type Language = Text


-- | Add language parameter
language ∷ Language → Request a f
language l = wrap $ __query %~ M.insert "language" l
{-# INLINE language #-}


type Tag = Text


-- | Add tags parameter
tags ∷ [Tag] → Request a f
tags ts = wrap $ __query %~ M.insert "tags" (T.intercalate "," ts)
{-# INLINE tags #-}


-- | Add tag parameter
tag ∷ Tag → Request a f
tag t = wrap $ __query %~ M.insert "tag" t
{-# INLINE tag #-}


type Autocorrect = Bool


-- | Add autocorrect parameter
autocorrect ∷ Autocorrect → Request a f
autocorrect au = wrap $ __query %~ M.insert "tags" (if au then "1" else "0")
{-# INLINE autocorrect #-}


type Page = Int


-- | Add page parameter
page ∷ Page → Request a f
page p = wrap $ __query %~ M.insert "page" (T.pack $ show p)
{-# INLINE page #-}


type Limit = Int


-- | Add limit parameter
limit ∷ Limit → Request a f
limit l = wrap $ __query %~ M.insert "limit" (T.pack $ show l)
{-# INLINE limit #-}


type Message = Text


-- | Add message parameter
message ∷ Message → Request a f
message m = wrap $ __query %~ M.insert "message" m
{-# INLINE message #-}


type Public = Bool


-- | Add public parameter
public ∷ Public → Request a f
public p = wrap $ __query %~ M.insert "public" (if p then "1" else "0")
{-# INLINE public #-}


type Recipient = Text


-- | Add recipient parameter
recipient ∷ Recipient → Request a f
recipient r = wrap $ __query %~ M.insert "recipient" r
{-# INLINE recipient #-}


type Username = Text


-- | Add username parameter
username ∷ Username → Request a f
username u = wrap $ __query %~ M.insert "username" u
{-# INLINE username #-}


type User = Text


-- | Add user parameter
user ∷ User → Request a f
user u = wrap $ __query %~ M.insert "user" u
{-# INLINE user #-}


-- | Add type parameter
type' ∷ Int → Text → Request a f
type' n t = wrap $ __query %~ M.insert ("type" <> T.pack (show n)) t
{-# INLINE type' #-}


-- | Add value parameter
value ∷ Int → Text → Request a f
value n v = wrap $ __query %~ M.insert ("value" <> T.pack (show n)) v
{-# INLINE value #-}
