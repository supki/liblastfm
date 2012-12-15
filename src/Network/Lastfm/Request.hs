{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request construction
module Network.Lastfm.Request
  ( -- * Request
    Request, R, Auth(..), Format(..), Response
    -- * Request major parameters
  , api, post, get, json, xml, APIKey, apiKey, SessionKey, sessionKey
    -- * Request minor parameters
  , Token, token, Callback, callback
  , Artist, artist, Album, album, MBID, mbid
  , Country, country, Autocorrect, autocorrect
  , Event, event, Status(..), status
  , From, from, To, to, Group, group
  , Language, language, Distance, distance
  , Longitude, longitude, Latitude, latitude, Location, location
  , Start, start, End, end, FestivalsOnly, festivalsonly
  , StartTimestamp, startTimestamp, EndTimestamp, endTimestamp
  , Metro, metro, Tag, tags, tag, Track, track, Timestamp , timestamp, Fingerprint, fingerprint
  , AlbumArtist, albumArtist, Duration, duration, TrackNumber, trackNumber
  , ChosenByUser, chosenByUser, Context, context, StreamId, streamId
  , RecentTracks, recentTracks
  , Recipient, recipient, Username, username, User, user, Password, password
  , Public, public, Message, message, Page, page, Limit, limit
  , TaggingType, taggingType, UseRecs, useRecs, Venue, venue, VenueName, venueName
  , type', value
  ) where

import Data.Int (Int64)
import Data.Monoid ((<>))

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.Builder.Int as T

import Network.Lastfm.Internal


data APIKey
data SessionKey


-- | Change request API key
apiKey ∷ Text → Request f a APIKey
apiKey = add "api_key"
{-# INLINE apiKey #-}


-- | Change request session key
sessionKey ∷ Text → Request f RequireSign SessionKey
sessionKey = add "sk"
{-# INLINE sessionKey #-}


type Token = Text


-- | Add token parameter
token ∷ Token → Request f a t
token = add "token"
{-# INLINE token #-}


type Callback = Text


-- | Add callback link parameter
callback ∷ Callback → Request f a t
callback = add "cb"
{-# INLINE callback #-}


type Artist = Text


-- | Add artist parameter
artist ∷ Artist → Request f a t
artist = add "artist"
{-# INLINE artist #-}


type Album = Text


-- | Add artist parameter
album ∷ Album → Request f a t
album = add "album"
{-# INLINE album #-}


type MBID = Text


-- | Add MBID parameter
mbid ∷ MBID → Request f a t
mbid = add "mbid"
{-# INLINE mbid #-}


type Country = Text


-- | Add country parameter
country ∷ Country → Request f a t
country = add "country"
{-# INLINE country #-}


type Language = Text


-- | Add language parameter
language ∷ Language → Request f a t
language = add "lang"
{-# INLINE language #-}


type Tag = Text


-- | Add tags parameter
tags ∷ [Tag] → Request f a t
tags = add "tags" . T.intercalate ","
{-# INLINE tags #-}


-- | Add tag parameter
tag ∷ Tag → Request f a t
tag = add "tag"
{-# INLINE tag #-}


type Autocorrect = Bool


-- | Add autocorrect parameter
autocorrect ∷ Autocorrect → Request f a t
autocorrect = add "tags" . boolToText
{-# INLINE autocorrect #-}


type Page = Int64


-- | Add page parameter
page ∷ Page → Request f a t
page = add "page" . T.toLazyText . T.decimal
{-# INLINE page #-}


type Limit = Int64


-- | Add limit parameter
limit ∷ Limit → Request f a t
limit = add "limit" . T.toLazyText . T.decimal
{-# INLINE limit #-}


type Message = Text


-- | Add message parameter
message ∷ Message → Request f a t
message = add "message"
{-# INLINE message #-}


type Public = Bool


-- | Add public parameter
public ∷ Public → Request f a t
public = add "public" . boolToText
{-# INLINE public #-}


type Recipient = Text


-- | Add recipient parameter
recipient ∷ Recipient → Request f a t
recipient = add "recipient"
{-# INLINE recipient #-}


type Username = Text


-- | Add username parameter
username ∷ Username → Request f a t
username = add "username"
{-# INLINE username #-}


type User = Text


-- | Add user parameter
user ∷ User → Request f a t
user = add "user"
{-# INLINE user #-}


type Password = Text


-- | Add password parameter
password ∷ Password → Request f a t
password = add "password"
{-# INLINE password #-}


data Status = Attending | Maybe | NotAttending


-- | Add status parameter
status ∷ Status → Request f a t
status = add "status" . \s -> case s of
  Attending → "0"
  Maybe     → "1"
  _         → "2"
{-# INLINE status #-}


type Event = Int64


-- | Add event parameter
event ∷ Event → Request f a t
event = add "event" . T.toLazyText . T.decimal
{-# INLINE event #-}


type Longitude = Text


-- | Add longitude parameter
longitude ∷ Longitude → Request f a t
longitude = add "longitude"
{-# INLINE longitude #-}


type Latitude = Text


-- | Add latitude parameter
latitude ∷ Latitude → Request f a t
latitude = add "latitude"
{-# INLINE latitude #-}


type Location = Text


-- | Add location parameter
location ∷ Location → Request f a t
location = add "location"
{-# INLINE location #-}


type Distance = Int64


-- | Add distance parameter
distance ∷ Distance → Request f a t
distance = add "distance" . T.toLazyText . T.decimal
{-# INLINE distance #-}


type FestivalsOnly = Bool


-- | Add festivalsonly parameter
festivalsonly ∷ FestivalsOnly → Request f a t
festivalsonly = add "festivalsonly" . boolToText
{-# INLINE festivalsonly #-}


type Start = Int64


-- | Add start parameter
start ∷ Start → Request f a t
start = add "start" . T.toLazyText . T.decimal
{-# INLINE start #-}


type End = Int64


-- | Add end parameter
end ∷ End → Request f a t
end = add "end" . T.toLazyText . T.decimal
{-# INLINE end #-}


type StartTimestamp = Int64


-- | Add startTimestamp parameter
startTimestamp ∷ StartTimestamp → Request f a t
startTimestamp = add "startTimestamp" . T.toLazyText . T.decimal
{-# INLINE startTimestamp #-}


type EndTimestamp = Int64


-- | Add endTimestamp parameter
endTimestamp ∷ EndTimestamp → Request f a t
endTimestamp = add "endTimestamp" . T.toLazyText . T.decimal
{-# INLINE endTimestamp #-}


type Metro = Text


-- | Add metro parameter
metro ∷ Metro → Request f a t
metro = add "metro"
{-# INLINE metro #-}


type From = Int64


-- | Add from parameter
from ∷ From → Request f a t
from = add "from" . T.toLazyText . T.decimal
{-# INLINE from #-}


type To = Int64


-- | Add to parameter
to ∷ To → Request f a t
to = add "to" . T.toLazyText . T.decimal
{-# INLINE to #-}


-- | Add type parameter
type' ∷ Int64 → Text → Request f a t
type' n = add ("type" <> T.toLazyText (T.decimal n))
{-# INLINE type' #-}


-- | Add value parameter
value ∷ Int64 → Text → Request f a t
value n = add ("value" <> T.toLazyText (T.decimal n))
{-# INLINE value #-}


type Track = Text


-- | Add track parameter
track ∷ Track → Request f a t
track = add "track"
{-# INLINE track #-}


type Timestamp = Int64


-- | Add timestamp parameter
timestamp ∷ Timestamp → Request f a t
timestamp = add "timestamp" . T.toLazyText . T.decimal
{-# INLINE timestamp #-}


type Fingerprint = Int64


-- | Add fingerprint parameter
fingerprint ∷ Fingerprint → Request f a t
fingerprint = add "fingerprintid" . T.toLazyText . T.decimal
{-# INLINE fingerprint #-}


type AlbumArtist = Text


-- | Add albumArtist parameter
albumArtist ∷ AlbumArtist → Request f a t
albumArtist = add "albumArtist"
{-# INLINE albumArtist #-}


type Context = Text


-- | Add context parameter
context ∷ Context → Request f a t
context = add "context"
{-# INLINE context #-}


type StreamId = Int64


-- | Add streamId parameter
streamId ∷ StreamId → Request f a t
streamId = add "streamId" . T.toLazyText . T.decimal
{-# INLINE streamId #-}


type Duration = Int64


-- | Add duration parameter
duration ∷ Duration → Request f a t
duration = add "duration" . T.toLazyText . T.decimal
{-# INLINE duration #-}


type TrackNumber = Int64


-- | Add trackNumber parameter
trackNumber ∷ TrackNumber → Request f a t
trackNumber = add "trackNumber" . T.toLazyText . T.decimal
{-# INLINE trackNumber #-}


type ChosenByUser = Bool


-- | Add chosenByUser parameter
chosenByUser ∷ ChosenByUser → Request f a t
chosenByUser = add "chosenByUser" . boolToText
{-# INLINE chosenByUser #-}


type TaggingType = Text


-- | Add taggingType parameter
taggingType ∷ TaggingType → Request f a t
taggingType = add "taggingtype"
{-# INLINE taggingType #-}


type RecentTracks = Bool


-- | Add recentTracks parameter
recentTracks ∷ RecentTracks → Request f a t
recentTracks = add "recentTracks" . boolToText
{-# INLINE recentTracks #-}


type UseRecs = Bool


-- | Add useRecs parameter
useRecs ∷ UseRecs → Request f a t
useRecs = add "useRecs" . boolToText
{-# INLINE useRecs #-}


type Group = Text


-- | Add group parameter
group ∷ Group → Request f a t
group = add "group"
{-# INLINE group #-}


type Venue = Int64


-- | Add venue parameter
venue ∷ Venue → Request f a t
venue = add "venue" . T.toLazyText . T.decimal
{-# INLINE venue #-}


type VenueName = Text


-- | Add venue parameter
venueName ∷ VenueName → Request f a t
venueName = add "venue"
{-# INLINE venueName #-}


boolToText ∷ Bool → Text
boolToText b = if b then "1" else "0"
{-# INLINE boolToText #-}
