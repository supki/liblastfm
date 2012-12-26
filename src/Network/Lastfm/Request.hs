{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request construction
module Network.Lastfm.Request
  ( -- * Request
    Request, R, Auth(..), Format(..)
    -- * Request major parameters
  , api, post, get, json, xml, Ready, APIKey, apiKey, SessionKey, sessionKey
    -- * Request minor parameters
  , ArtistAlbumOrMBID, ArtistOrMBID, ArtistTrackOrMBID
  , Token, token, Callback, callback
  , Artist, artist, Album, album, MBID, mbid
  , Country, country, Autocorrect, autocorrect
  , Event, event, Status(..), status
  , From, from, To, to, Group, group
  , Language, language, Distance, distance
  , Longitude, longitude, Latitude, latitude, Location, location
  , Start, start, End, end, Festivals, festivalsonly
  , StartTimestamp, startTimestamp, EndTimestamp, endTimestamp
  , Metro, metro, Tag, tags, tag, Track, track, Timestamp , timestamp, Fingerprint, fingerprint
  , AlbumArtist, albumArtist, Duration, duration, TrackNumber, trackNumber
  , Playlist, playlist, Title, title, Description, description
  , ChosenByUser, chosenByUser, Context, context, StreamId, streamId
  , RecentTracks, recentTracks
  , Recipient, recipient, Username, username, User, user, Password, password
  , Public, public, Message, message, Page, page, Limit, limit
  , TaggingType, taggingType, UseRecs, useRecs, Venue, venue, VenueName, venueName
  , Discovery, discovery, RTP, rtp, BuyLinks, buyLinks, Multiplier(..), multiplier
  , Bitrate(..), bitrate, Name, name, Station, station
  , type', Value', value
  ) where

import Data.Int (Int64)
import Data.Monoid ((<>))

import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as T

import Network.Lastfm.Internal


class ArtistAlbumOrMBID a

instance ArtistAlbumOrMBID MBID
instance ArtistAlbumOrMBID (Artist, Album)

class ArtistTrackOrMBID a

instance ArtistTrackOrMBID MBID
instance ArtistTrackOrMBID (Artist, Track)

class ArtistOrMBID a

instance ArtistOrMBID MBID
instance ArtistOrMBID Artist


class Argument a where
  add ∷ Text → a → Request f b t
  add k v = wrap $ \r@R { query = q } → r { query = M.insert k (toText v) q }
  {-# INLINE add #-}

  toText ∷ a → Text

instance Argument Text where
  toText = id
  {-# INLINE toText #-}

instance Argument Bool where
  toText b = if b then "1" else "0"
  {-# INLINE toText #-}

instance Argument Int64 where
  toText = T.toLazyText . T.decimal
  {-# INLINE toText #-}

instance Argument a ⇒ Argument [a] where
  toText = T.intercalate "," . map toText
  {-# INLINE toText #-}


data APIKey
data SessionKey
data Token
data Callback


-- | Change request API method
--
-- Primarily used in API call wrappers, not intended for usage by library user
api ∷ Text → Request f a t
api = add "method"
{-# INLINE api #-}

-- | Change html method to GET
--
-- Primarily used in API call wrappers, not intended for usage by library user
get ∷ Request f a t
get = wrap $ \r → r { method = "GET" }
{-# INLINE get #-}

-- | Change html method to POST
--
-- Primarily used in API call wrappers, not intended for usage by library user
post ∷ Request f a t
post = wrap $ \r → r { method = "POST" }
{-# INLINE post #-}

-- | Change API response format to JSON
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
json ∷ Request JSON a t
json = wrap id
{-# INLINE json #-}

-- | Change API response format to XML
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
xml ∷ Request XML a t
xml = wrap id
{-# INLINE xml #-}

-- | Change request API key
apiKey ∷ Text → Request f a APIKey
apiKey = add "api_key"
{-# INLINE apiKey #-}

-- | Change request session key
sessionKey ∷ Text → Request f Sign SessionKey
sessionKey = add "sk"
{-# INLINE sessionKey #-}

-- | Add token parameter
token ∷ Text → Request f a Token
token = add "token"
{-# INLINE token #-}

-- | Add callback link parameter
callback ∷ Text → Request f a Callback
callback = add "cb"
{-# INLINE callback #-}


data Artist
data Album
data MBID
data Country
data Language
data Tag
data Autocorrect
data Page
data Limit
data Message
data Public
data Recipient
data Username
data User
data Password
data Status = Attending | Maybe | NotAttending
data Event
data Festivals
data Longitude
data Latitude
data Location
data Distance
data Metro
data Start
data End
data StartTimestamp
data EndTimestamp
data From
data To
data Value'
data Playlist
data Title
data Description
data Track
data Timestamp
data Fingerprint
data AlbumArtist
data Context
data StreamId
data Duration
data TrackNumber
data ChosenByUser
data TaggingType
data RecentTracks
data UseRecs
data Group
data Venue
data VenueName
data Multiplier = M1 | M2
data Bitrate = B64 | B128
data Name
data Station
data Discovery
data RTP
data BuyLinks


-- | Add artist parameter
artist ∷ Text → Request f a Artist
artist = add "artist"
{-# INLINE artist #-}

-- | Add artist parameter
album ∷ Text → Request f a Album
album = add "album"
{-# INLINE album #-}

-- | Add MBID parameter
mbid ∷ Text → Request f a MBID
mbid = add "mbid"
{-# INLINE mbid #-}

-- | Add country parameter
country ∷ Text → Request f a Country
country = add "country"
{-# INLINE country #-}

-- | Add language parameter
language ∷ Text → Request f a Language
language = add "lang"
{-# INLINE language #-}

-- | Add tags parameter
tags ∷ [Text] → Request f a [Tag]
tags = add "tags"
{-# INLINE tags #-}

-- | Add tag parameter
tag ∷ Text → Request f a Tag
tag = add "tag"
{-# INLINE tag #-}

-- | Add autocorrect parameter
autocorrect ∷ Bool → Request f a Autocorrect
autocorrect = add "tags"
{-# INLINE autocorrect #-}

-- | Add page parameter
page ∷ Int64 → Request f a Page
page = add "page"
{-# INLINE page #-}

-- | Add limit parameter
limit ∷ Int64 → Request f a Limit
limit = add "limit"
{-# INLINE limit #-}

-- | Add message parameter
message ∷ Text → Request f a Message
message = add "message"
{-# INLINE message #-}

-- | Add public parameter
public ∷ Bool → Request f a Public
public = add "public"
{-# INLINE public #-}

-- | Add recipient parameter
recipient ∷ Text → Request f a Recipient
recipient = add "recipient"
{-# INLINE recipient #-}

-- | Add username parameter
username ∷ Text → Request f a Username
username = add "username"
{-# INLINE username #-}

-- | Add user parameter
user ∷ Text → Request f a User
user = add "user"
{-# INLINE user #-}

-- | Add password parameter
password ∷ Text → Request f a Password
password = add "password"
{-# INLINE password #-}

-- | Add status parameter
status ∷ Status → Request f a Status
status = add "status" . T.pack . \s -> case s of
  Attending → "0"
  Maybe     → "1"
  _         → "2"
{-# INLINE status #-}

-- | Add event parameter
event ∷ Int64 → Request f a Event
event = add "event"
{-# INLINE event #-}

-- | Add festivalsonly parameter
festivalsonly ∷ Bool → Request f a Festivals
festivalsonly = add "festivalsonly"
{-# INLINE festivalsonly #-}

-- | Add longitude parameter
longitude ∷ Text → Request f a Longitude
longitude = add "longitude"
{-# INLINE longitude #-}

-- | Add latitude parameter
latitude ∷ Text → Request f a Latitude
latitude = add "latitude"
{-# INLINE latitude #-}

-- | Add location parameter
location ∷ Text → Request f a Location
location = add "location"
{-# INLINE location #-}

-- | Add distance parameter
distance ∷ Int64 → Request f a Distance
distance = add "distance"
{-# INLINE distance #-}

-- | Add venue parameter
venue ∷ Int64 → Request f a Venue
venue = add "venue"
{-# INLINE venue #-}

-- | Add venue parameter
venueName ∷ Text → Request f a VenueName
venueName = add "venue"
{-# INLINE venueName #-}

-- | Add metro parameter
metro ∷ Text → Request f a Metro
metro = add "metro"
{-# INLINE metro #-}

-- | Add start parameter
start ∷ Int64 → Request f a Start
start = add "start"
{-# INLINE start #-}

-- | Add end parameter
end ∷ Int64 → Request f a End
end = add "end"
{-# INLINE end #-}

-- | Add startTimestamp parameter
startTimestamp ∷ Int64 → Request f a StartTimestamp
startTimestamp = add "startTimestamp"
{-# INLINE startTimestamp #-}

-- | Add endTimestamp parameter
endTimestamp ∷ Int64 → Request f a EndTimestamp
endTimestamp = add "endTimestamp"
{-# INLINE endTimestamp #-}

-- | Add from parameter
from ∷ Int64 → Request f a From
from = add "from"
{-# INLINE from #-}

-- | Add to parameter
to ∷ Int64 → Request f a To
to = add "to"
{-# INLINE to #-}

-- | Add type parameter
type' ∷ Int64 → Text → Request f a t
type' n = add ("type" <> toText n)
{-# INLINE type' #-}

-- | Add value parameter
value ∷ Int64 → Text → Request f a Value'
value n = add ("value" <> toText n)
{-# INLINE value #-}

-- | Add track parameter
track ∷ Text → Request f a Track
track = add "track"
{-# INLINE track #-}

-- | Add timestamp parameter
timestamp ∷ Int64 → Request f a Timestamp
timestamp = add "timestamp"
{-# INLINE timestamp #-}

-- | Add playlistID parameter
playlist ∷ Int64 → Request f a Playlist
playlist = add "playlistID"
{-# INLINE playlist #-}

-- | Add title parameter
title ∷ Text → Request f a Title
title = add "title"
{-# INLINE title #-}

-- | Add description parameter
description ∷ Text → Request f a Description
description = add "description"
{-# INLINE description #-}

-- | Add fingerprint parameter
fingerprint ∷ Int64 → Request f a Fingerprint
fingerprint = add "fingerprintid"
{-# INLINE fingerprint #-}

-- | Add albumArtist parameter
albumArtist ∷ Text → Request f a AlbumArtist
albumArtist = add "albumArtist"
{-# INLINE albumArtist #-}

-- | Add context parameter
context ∷ Text → Request f a Context
context = add "context"
{-# INLINE context #-}

-- | Add streamId parameter
streamId ∷ Int64 → Request f a StreamId
streamId = add "streamId"
{-# INLINE streamId #-}

-- | Add duration parameter
duration ∷ Int64 → Request f a Duration
duration = add "duration"
{-# INLINE duration #-}

-- | Add trackNumber parameter
trackNumber ∷ Int64 → Request f a TrackNumber
trackNumber = add "trackNumber"
{-# INLINE trackNumber #-}

-- | Add chosenByUser parameter
chosenByUser ∷ Bool → Request f a ChosenByUser
chosenByUser = add "chosenByUser"
{-# INLINE chosenByUser #-}

-- | Add taggingType parameter
taggingType ∷ Text → Request f a TaggingType
taggingType = add "taggingtype"
{-# INLINE taggingType #-}

-- | Add recentTracks parameter
recentTracks ∷ Bool → Request f a RecentTracks
recentTracks = add "recentTracks"
{-# INLINE recentTracks #-}

-- | Add useRecs parameter
useRecs ∷ Bool → Request f a UseRecs
useRecs = add "useRecs"
{-# INLINE useRecs #-}

-- | Add group parameter
group ∷ Text → Request f a Group
group = add "group"
{-# INLINE group #-}

-- | Add multiplier parameter
multiplier ∷ Multiplier → Request f a Multiplier
multiplier m = case m of
  M1 → add "speed_multiplier" (T.pack "1.0")
  M2 → add "speed_multiplier" (T.pack "2.0")
{-# INLINE multiplier #-}

-- | Add bitrate parameter
bitrate ∷ Bitrate → Request f a Bitrate
bitrate b = case b of
  B64  → add "bitrate" (64 ∷ Int64)
  B128 → add "bitrate" (128 ∷ Int64)
{-# INLINE bitrate #-}

-- | Add name parameter
name ∷ Text → Request f a Name
name = add "name"
{-# INLINE name #-}

-- | Add station parameter
station ∷ Text → Request f a Station
station = add "station"
{-# INLINE station #-}

-- | Add group parameter
discovery ∷ Bool → Request f a Discovery
discovery = add "discovery"
{-# INLINE discovery #-}

-- | Add rtp parameter
rtp ∷ Bool → Request f a RTP
rtp = add "rtp"
{-# INLINE rtp #-}

-- | Add buyLinks parameter
buyLinks ∷ Bool → Request f a BuyLinks
buyLinks = add "buyLinks"
{-# INLINE buyLinks #-}
