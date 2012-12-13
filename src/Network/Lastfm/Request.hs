{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request construction
module Network.Lastfm.Request
  ( -- * Request
    Request, R, Auth(..), Format(..), Response
    -- * Request major parameters
  , api, post, get, json, xml, apiKey, sessionKey
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


-- | Change request API key
apiKey ∷ Text → Request a f
apiKey = add "api_key"
{-# INLINE apiKey #-}


-- | Change request session key
sessionKey ∷ Text → Request a f
sessionKey = add "sk"
{-# INLINE sessionKey #-}


type Token = Text


-- | Add token parameter
token ∷ Token → Request a f
token = add "token"
{-# INLINE token #-}


type Callback = Text


-- | Add callback link parameter
callback ∷ Callback → Request a f
callback = add "cb"
{-# INLINE callback #-}


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


type Page = Int64


-- | Add page parameter
page ∷ Page → Request a f
page = add "page" . T.toLazyText . T.decimal
{-# INLINE page #-}


type Limit = Int64


-- | Add limit parameter
limit ∷ Limit → Request a f
limit = add "limit" . T.toLazyText . T.decimal
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


type Password = Text


-- | Add password parameter
password ∷ Password → Request a f
password = add "password"
{-# INLINE password #-}


data Status = Attending | Maybe | NotAttending


-- | Add status parameter
status ∷ Status → Request a f
status = add "status" . \s -> case s of
  Attending → "0"
  Maybe     → "1"
  _         → "2"
{-# INLINE status #-}


type Event = Int64


-- | Add event parameter
event ∷ Event → Request a f
event = add "event" . T.toLazyText . T.decimal
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


type Distance = Int64


-- | Add distance parameter
distance ∷ Distance → Request a f
distance = add "distance" . T.toLazyText . T.decimal
{-# INLINE distance #-}


type FestivalsOnly = Bool


-- | Add festivalsonly parameter
festivalsonly ∷ FestivalsOnly → Request a f
festivalsonly = add "festivalsonly" . boolToText
{-# INLINE festivalsonly #-}


type Start = Int64


-- | Add start parameter
start ∷ Start → Request a f
start = add "start" . T.toLazyText . T.decimal
{-# INLINE start #-}


type End = Int64


-- | Add end parameter
end ∷ End → Request a f
end = add "end" . T.toLazyText . T.decimal
{-# INLINE end #-}


type StartTimestamp = Int64


-- | Add startTimestamp parameter
startTimestamp ∷ StartTimestamp → Request a f
startTimestamp = add "startTimestamp" . T.toLazyText . T.decimal
{-# INLINE startTimestamp #-}


type EndTimestamp = Int64


-- | Add endTimestamp parameter
endTimestamp ∷ EndTimestamp → Request a f
endTimestamp = add "endTimestamp" . T.toLazyText . T.decimal
{-# INLINE endTimestamp #-}


type Metro = Text


-- | Add metro parameter
metro ∷ Metro → Request a f
metro = add "metro"
{-# INLINE metro #-}


type From = Int64


-- | Add from parameter
from ∷ From → Request a f
from = add "from" . T.toLazyText . T.decimal
{-# INLINE from #-}


type To = Int64


-- | Add to parameter
to ∷ To → Request a f
to = add "to" . T.toLazyText . T.decimal
{-# INLINE to #-}


-- | Add type parameter
type' ∷ Int64 → Text → Request a f
type' n = add ("type" <> T.toLazyText (T.decimal n))
{-# INLINE type' #-}


-- | Add value parameter
value ∷ Int64 → Text → Request a f
value n = add ("value" <> T.toLazyText (T.decimal n))
{-# INLINE value #-}


type Track = Text


-- | Add track parameter
track ∷ Track → Request a f
track = add "track"
{-# INLINE track #-}


type Timestamp = Int64


-- | Add timestamp parameter
timestamp ∷ Timestamp → Request a f
timestamp = add "timestamp" . T.toLazyText . T.decimal
{-# INLINE timestamp #-}


type Fingerprint = Int64


-- | Add fingerprint parameter
fingerprint ∷ Fingerprint → Request a f
fingerprint = add "fingerprintid" . T.toLazyText . T.decimal
{-# INLINE fingerprint #-}


type AlbumArtist = Text


-- | Add albumArtist parameter
albumArtist ∷ AlbumArtist → Request a f
albumArtist = add "albumArtist"
{-# INLINE albumArtist #-}


type Context = Text


-- | Add context parameter
context ∷ Context → Request a f
context = add "context"
{-# INLINE context #-}


type StreamId = Int64


-- | Add streamId parameter
streamId ∷ StreamId → Request a f
streamId = add "streamId" . T.toLazyText . T.decimal
{-# INLINE streamId #-}


type Duration = Int64


-- | Add duration parameter
duration ∷ Duration → Request a f
duration = add "duration" . T.toLazyText . T.decimal
{-# INLINE duration #-}


type TrackNumber = Int64


-- | Add trackNumber parameter
trackNumber ∷ TrackNumber → Request a f
trackNumber = add "trackNumber" . T.toLazyText . T.decimal
{-# INLINE trackNumber #-}


type ChosenByUser = Bool


-- | Add chosenByUser parameter
chosenByUser ∷ ChosenByUser → Request a f
chosenByUser = add "chosenByUser" . boolToText
{-# INLINE chosenByUser #-}


type TaggingType = Text


-- | Add taggingType parameter
taggingType ∷ TaggingType → Request a f
taggingType = add "taggingtype"
{-# INLINE taggingType #-}


type RecentTracks = Bool


-- | Add recentTracks parameter
recentTracks ∷ RecentTracks → Request a f
recentTracks = add "recentTracks" . boolToText
{-# INLINE recentTracks #-}


type UseRecs = Bool


-- | Add useRecs parameter
useRecs ∷ UseRecs → Request a f
useRecs = add "useRecs" . boolToText
{-# INLINE useRecs #-}


type Group = Text


-- | Add group parameter
group ∷ Group → Request a f
group = add "group"
{-# INLINE group #-}


type Venue = Int64


-- | Add venue parameter
venue ∷ Venue → Request a f
venue = add "venue" . T.toLazyText . T.decimal
{-# INLINE venue #-}


type VenueName = Text


-- | Add venue parameter
venueName ∷ VenueName → Request a f
venueName = add "venue"
{-# INLINE venueName #-}


boolToText ∷ Bool → Text
boolToText b = if b then "1" else "0"
{-# INLINE boolToText #-}
