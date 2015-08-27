{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Request construction
module Network.Lastfm.Request
  ( -- * Request
    Request, R, Ready, Sign, Format(..)
    -- * Request major parameters
  , api, post, get, json, xml, APIKey, apiKey, SessionKey, sessionKey
    -- * Request minor parameters
  , Token, token, Callback, callback
  , Artist, artist, artists, Album, album, MBID, mbid
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
  , Targeted, comparison, Scrobble, LibraryAlbum, LibraryArtist
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Data.Int (Int64)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (mempty)
#endif
import           Data.Monoid ((<>))
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.Lastfm.Internal


class Argument a where
  add :: Text -> a -> Request f b
  add k v = wrap $ \r@R { _query = q } -> r { _query = M.insert k (toText v) q }
  {-# INLINE add #-}

  toText :: a -> Text

instance Argument Text where
  toText = id
  {-# INLINE toText #-}

instance Argument Bool where
  toText b = if b then "1" else "0"
  {-# INLINE toText #-}

instance Argument Int64 where
  toText = T.pack . show
  {-# INLINE toText #-}

instance Argument a => Argument [a] where
  toText = T.intercalate "," . map toText
  {-# INLINE toText #-}


data APIKey
data SessionKey
data Token
data Callback


-- | Change request API method
--
-- Primarily used in API call wrappers, not intended for usage by library user
api :: Text -> Request f a
api = add "method"
{-# INLINE api #-}

-- | Change html _method to GET
--
-- Primarily used in API call wrappers, not intended for usage by library user
get :: Request f a
get = wrap $ \r -> r { _method = "GET" }
{-# INLINE get #-}

-- | Change html _method to POST
--
-- Primarily used in API call wrappers, not intended for usage by library user
post :: Request f a
post = wrap $ \r -> r { _method = "POST" }
{-# INLINE post #-}

-- | Change API response format to JSON
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
json :: Request 'JSON a
json = wrap id
{-# INLINE json #-}

-- | Change API response format to XML
--
-- This is a little helper. It's actually enough
-- to specialize Format manually
xml :: Request 'XML a
xml = wrap id
{-# INLINE xml #-}

-- | Change request API key
apiKey :: Text -> Request f APIKey
apiKey = add "api_key"
{-# INLINE apiKey #-}

-- | Change request session key
sessionKey :: Text -> Request f SessionKey
sessionKey = add "sk"
{-# INLINE sessionKey #-}

-- | Add token parameter
token :: Text -> Request f Token
token = add "token"
{-# INLINE token #-}

-- | Add callback link parameter
callback :: Text -> Request f Callback
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
data Scrobble
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
data LibraryAlbum
data LibraryArtist


-- | Add artist parameter
artist :: Text -> Request f Artist
artist = add "artist"
{-# INLINE artist #-}

-- | Add artists parameter
artists :: [Text] -> Request f [Artist]
artists = add "artists"
{-# INLINE artists #-}

-- | Add album parameter
album :: Text -> Request f Album
album = add "album"
{-# INLINE album #-}

-- | Add MBID parameter
mbid :: Text -> Request f MBID
mbid = add "mbid"
{-# INLINE mbid #-}

-- | Add country parameter
country :: Text -> Request f Country
country = add "country"
{-# INLINE country #-}

-- | Add language parameter
language :: Text -> Request f Language
language = add "lang"
{-# INLINE language #-}

-- | Add tags parameter
tags :: [Text] -> Request f [Tag]
tags = add "tags"
{-# INLINE tags #-}

-- | Add tag parameter
tag :: Text -> Request f Tag
tag = add "tag"
{-# INLINE tag #-}

-- | Add autocorrect parameter
autocorrect :: Bool -> Request f Autocorrect
autocorrect = add "tags"
{-# INLINE autocorrect #-}

-- | Add page parameter
page :: Int64 -> Request f Page
page = add "page"
{-# INLINE page #-}

-- | Add limit parameter
limit :: Int64 -> Request f Limit
limit = add "limit"
{-# INLINE limit #-}

-- | Add message parameter
message :: Text -> Request f Message
message = add "message"
{-# INLINE message #-}

-- | Add public parameter
public :: Bool -> Request f Public
public = add "public"
{-# INLINE public #-}

-- | Add recipient parameter
recipient :: Text -> Request f Recipient
recipient = add "recipient"
{-# INLINE recipient #-}

-- | Add username parameter
username :: Text -> Request f Username
username = add "username"
{-# INLINE username #-}

-- | Add user parameter
user :: Text -> Request f User
user = add "user"
{-# INLINE user #-}

-- | Add password parameter
password :: Text -> Request f Password
password = add "password"
{-# INLINE password #-}

-- | Add status parameter
status :: Status -> Request f Status
status = add "status" . T.pack . \s -> case s of
  Attending -> "0"
  Maybe     -> "1"
  _         -> "2"
{-# INLINE status #-}

-- | Add event parameter
event :: Int64 -> Request f Event
event = add "event"
{-# INLINE event #-}

-- | Add festivalsonly parameter
festivalsonly :: Bool -> Request f Festivals
festivalsonly = add "festivalsonly"
{-# INLINE festivalsonly #-}

-- | Add longitude parameter
longitude :: Text -> Request f Longitude
longitude = add "longitude"
{-# INLINE longitude #-}

-- | Add latitude parameter
latitude :: Text -> Request f Latitude
latitude = add "latitude"
{-# INLINE latitude #-}

-- | Add location parameter
location :: Text -> Request f Location
location = add "location"
{-# INLINE location #-}

-- | Add distance parameter
distance :: Int64 -> Request f Distance
distance = add "distance"
{-# INLINE distance #-}

-- | Add venue parameter
venue :: Int64 -> Request f Venue
venue = add "venue"
{-# INLINE venue #-}

-- | Add venue parameter
venueName :: Text -> Request f VenueName
venueName = add "venue"
{-# INLINE venueName #-}

-- | Add metro parameter
metro :: Text -> Request f Metro
metro = add "metro"
{-# INLINE metro #-}

-- | Add start parameter
start :: Int64 -> Request f Start
start = add "start"
{-# INLINE start #-}

-- | Add end parameter
end :: Int64 -> Request f End
end = add "end"
{-# INLINE end #-}

-- | Add startTimestamp parameter
startTimestamp :: Int64 -> Request f StartTimestamp
startTimestamp = add "startTimestamp"
{-# INLINE startTimestamp #-}

-- | Add endTimestamp parameter
endTimestamp :: Int64 -> Request f EndTimestamp
endTimestamp = add "endTimestamp"
{-# INLINE endTimestamp #-}

-- | Add from parameter
from :: Int64 -> Request f From
from = add "from"
{-# INLINE from #-}

-- | Add to parameter
to :: Int64 -> Request f To
to = add "to"
{-# INLINE to #-}

-- | Add track parameter
track :: Text -> Request f Track
track = add "track"
{-# INLINE track #-}

-- | Add timestamp parameter
timestamp :: Int64 -> Request f Timestamp
timestamp = add "timestamp"
{-# INLINE timestamp #-}

-- | Add playlistID parameter
playlist :: Int64 -> Request f Playlist
playlist = add "playlistID"
{-# INLINE playlist #-}

-- | Add title parameter
title :: Text -> Request f Title
title = add "title"
{-# INLINE title #-}

-- | Add description parameter
description :: Text -> Request f Description
description = add "description"
{-# INLINE description #-}

-- | Add fingerprint parameter
fingerprint :: Int64 -> Request f Fingerprint
fingerprint = add "fingerprintid"
{-# INLINE fingerprint #-}

-- | Add albumArtist parameter
albumArtist :: Text -> Request f AlbumArtist
albumArtist = add "albumArtist"
{-# INLINE albumArtist #-}

-- | Add context parameter
context :: Text -> Request f Context
context = add "context"
{-# INLINE context #-}

-- | Add streamId parameter
streamId :: Int64 -> Request f StreamId
streamId = add "streamId"
{-# INLINE streamId #-}

-- | Add duration parameter
duration :: Int64 -> Request f Duration
duration = add "duration"
{-# INLINE duration #-}

-- | Add trackNumber parameter
trackNumber :: Int64 -> Request f TrackNumber
trackNumber = add "trackNumber"
{-# INLINE trackNumber #-}

-- | Add chosenByUser parameter
chosenByUser :: Bool -> Request f ChosenByUser
chosenByUser = add "chosenByUser"
{-# INLINE chosenByUser #-}

-- | Add taggingType parameter
taggingType :: Text -> Request f TaggingType
taggingType = add "taggingtype"
{-# INLINE taggingType #-}

-- | Add recentTracks parameter
recentTracks :: Bool -> Request f RecentTracks
recentTracks = add "recentTracks"
{-# INLINE recentTracks #-}

-- | Add useRecs parameter
useRecs :: Bool -> Request f UseRecs
useRecs = add "useRecs"
{-# INLINE useRecs #-}

-- | Add group parameter
group :: Text -> Request f Group
group = add "group"
{-# INLINE group #-}

-- | Add multiplier parameter
multiplier :: Multiplier -> Request f Multiplier
multiplier m = case m of
  M1 -> add "speed_multiplier" (T.pack "1.0")
  M2 -> add "speed_multiplier" (T.pack "2.0")
{-# INLINE multiplier #-}

-- | Add bitrate parameter
bitrate :: Bitrate -> Request f Bitrate
bitrate b = case b of
  B64  -> add "bitrate" (64 :: Int64)
  B128 -> add "bitrate" (128 :: Int64)
{-# INLINE bitrate #-}

-- | Add name parameter
name :: Text -> Request f Name
name = add "name"
{-# INLINE name #-}

-- | Add station parameter
station :: Text -> Request f Station
station = add "station"
{-# INLINE station #-}

-- | Add group parameter
discovery :: Bool -> Request f Discovery
discovery = add "discovery"
{-# INLINE discovery #-}

-- | Add rtp parameter
rtp :: Bool -> Request f RTP
rtp = add "rtp"
{-# INLINE rtp #-}

-- | Add buyLinks parameter
buyLinks :: Bool -> Request f BuyLinks
buyLinks = add "buyLinks"
{-# INLINE buyLinks #-}


class Targeted a where
  target :: Request f a -> Text

instance Targeted [Artist] where
  target _ = "artists"
  {-# INLINE target #-}

instance Targeted User where
  target _ = "user"
  {-# INLINE target #-}

-- | Add comparison parameter
comparison :: Targeted a => Int64 -> Request f a -> Request f a
comparison n t = let z = target t in
  add ("type" <> toText n) z <*> add ("value" <> toText n) (_query (unwrap t (R mempty mempty mempty)) M.! z)
{-# INLINE comparison #-}
