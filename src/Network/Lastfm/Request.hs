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
  , Event, event, Status(..), status
  , From, from, To, to
  , Language, language, Distance, distance
  , Longitude, longitude, Latitude, latitude, Location, location
  , Start, start, End, end, FestivalsOnly, festivalsonly
  , Metro, metro
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
autocorrect = add "tags" . boolToText
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
public = add "public" . boolToText
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


data Status = Attending | Maybe | NotAttending deriving Show


-- | Add status parameter
status ∷ Status → Request a f
status s = add "status" (case s of Attending → "0"; Maybe → "1"; _ → "2")
{-# INLINE status #-}


type Event = Int


-- | Add event parameter
event ∷ Event → Request a f
event = add "event" . T.pack . show
{-# INLINE event #-}


type Longitude = Text


-- | Add longitude parameter
longitude ∷ Longitude → Request a f
longitude = add "longitude"
{-# INLINE longitude #-}


type Latitude = Text


-- | Add latitude parameter
latitude ∷ Latitude → Request a f
latitude = add "latitude"
{-# INLINE latitude #-}


type Location = Text


-- | Add location parameter
location ∷ Location → Request a f
location = add "location"
{-# INLINE location #-}


type Distance = Int


-- | Add distance parameter
distance ∷ Distance → Request a f
distance = add "distance" . T.pack . show
{-# INLINE distance #-}


type FestivalsOnly = Bool


-- | Add festivalsonly parameter
festivalsonly ∷ FestivalsOnly → Request a f
festivalsonly = add "festivalsonly" . boolToText
{-# INLINE festivalsonly #-}


type Start = Integer


-- | Add start parameter
start ∷ Start → Request a f
start = add "start" . T.pack . show
{-# INLINE start #-}


type End = Integer


-- | Add end parameter
end ∷ End → Request a f
end = add "end" . T.pack . show
{-# INLINE end #-}


type Metro = Text


-- | Add metro parameter
metro ∷ Metro → Request a f
metro = add "metro"
{-# INLINE metro #-}


type From = Integer


-- | Add from parameter
from ∷ From → Request a f
from = add "from" . T.pack . show
{-# INLINE from #-}


type To = Integer


-- | Add to parameter
to ∷ To → Request a f
to = add "to" . T.pack . show
{-# INLINE to #-}


-- | Add type parameter
type' ∷ Int → Text → Request a f
type' n = add ("type" <> T.pack (show n))
{-# INLINE type' #-}


-- | Add value parameter
value ∷ Int → Text → Request a f
value n = add ("value" <> T.pack (show n))
{-# INLINE value #-}


boolToText ∷ Bool → Text
boolToText b = (if b then "1" else "0")
