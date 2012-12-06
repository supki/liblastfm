{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request construction
module Network.Lastfm.Request
  ( -- * Request
    Request, R, Auth(..), Format(..), Response
    -- * Request major parameters
  , api, post, get, json, xml, apiKey
    -- * Request minor parameters
  , Artist, artist, Album, album, MBID, mbid
  , Country, country, Autocorrect, autocorrect
  , Language, language
  , Tag, tags, tag
  , Recipient, recipient, Username, username, User, user
  , Public, public, Message, message, Page, page, Limit, limit
  , type', value
  ) where

import Data.Monoid ((<>))

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Network.Lastfm.Internal


-- | Change request API key
--
-- Primarily used in API call wrappers, not intended for usage by library user
apiKey ∷ Text → Request a f
apiKey = add "api_key"
{-# INLINE apiKey #-}


type Artist = Text


-- | Add artist parameter
artist ∷ Artist → Request a f
artist = add "artist"
{-# INLINE artist #-}


type Album = Text


-- | Add artist parameter
album ∷ Album → Request a f
album = add "album"
{-# INLINE album #-}


type MBID = Text


-- | Add MBID parameter
mbid ∷ MBID → Request a f
mbid = add "mbid"
{-# INLINE mbid #-}


type Country = Text


-- | Add country parameter
country ∷ Country → Request a f
country = add "country"
{-# INLINE country #-}


type Language = Text


-- | Add language parameter
language ∷ Language → Request a f
language = add "lang"
{-# INLINE language #-}


type Tag = Text


-- | Add tags parameter
tags ∷ [Tag] → Request a f
tags = add "tags" . T.intercalate ","
{-# INLINE tags #-}


-- | Add tag parameter
tag ∷ Tag → Request a f
tag = add "tag"
{-# INLINE tag #-}


type Autocorrect = Bool


-- | Add autocorrect parameter
autocorrect ∷ Autocorrect → Request a f
autocorrect au = add "tags" (if au then "1" else "0")
{-# INLINE autocorrect #-}


type Page = Int


-- | Add page parameter
page ∷ Page → Request a f
page = add "page" . T.pack . show
{-# INLINE page #-}


type Limit = Int


-- | Add limit parameter
limit ∷ Limit → Request a f
limit = add "limit" . T.pack . show
{-# INLINE limit #-}


type Message = Text


-- | Add message parameter
message ∷ Message → Request a f
message = add "message"
{-# INLINE message #-}


type Public = Bool


-- | Add public parameter
public ∷ Public → Request a f
public p = add "public" (if p then "1" else "0")
{-# INLINE public #-}


type Recipient = Text


-- | Add recipient parameter
recipient ∷ Recipient → Request a f
recipient = add "recipient"
{-# INLINE recipient #-}


type Username = Text


-- | Add username parameter
username ∷ Username → Request a f
username = add "username"
{-# INLINE username #-}


type User = Text


-- | Add user parameter
user ∷ User → Request a f
user = add "user"
{-# INLINE user #-}


-- | Add type parameter
type' ∷ Int → Text → Request a f
type' n = add ("type" <> T.pack (show n))
{-# INLINE type' #-}


-- | Add value parameter
value ∷ Int → Text → Request a f
value n = add ("value" <> T.pack (show n))
{-# INLINE value #-}
