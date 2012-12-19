{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Request construction
module Network.Lastfm.Request
  ( -- * Request
    Request, R, Auth(..), Format(..), Response
    -- * Request major parameters
  , api, post, get, json, xml, Ready, APIKey, apiKey, SessionKey, sessionKey
    -- * Request minor parameters
  , ArtistAlbumOrMBID, ArtistOrMBID
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


class ArtistAlbumOrMBID a

instance ArtistAlbumOrMBID MBID
instance ArtistAlbumOrMBID (Artist, Album)


class ArtistOrMBID a

instance ArtistOrMBID MBID
instance ArtistOrMBID Artist


data Ready
data APIKey
data SessionKey


-- | Change request API key
apiKey ∷ Text → Request f a APIKey
apiKey = add "api_key"
{-# INLINE apiKey #-}


-- | Change request session key
sessionKey ∷ Text → Request f Sign SessionKey
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
data Venue
data VenueName


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
tags = add "tags" . T.intercalate ","
{-# INLINE tags #-}


-- | Add tag parameter
tag ∷ Text → Request f a Tag
tag = add "tag"
{-# INLINE tag #-}


-- | Add autocorrect parameter
autocorrect ∷ Bool → Request f a Autocorrect
autocorrect = add "tags" . boolToText
{-# INLINE autocorrect #-}


-- | Add page parameter
page ∷ Int64 → Request f a Page
page = add "page" . T.toLazyText . T.decimal
{-# INLINE page #-}


-- | Add limit parameter
limit ∷ Int64 → Request f a Limit
limit = add "limit" . T.toLazyText . T.decimal
{-# INLINE limit #-}


-- | Add message parameter
message ∷ Text → Request f a Message
message = add "message"
{-# INLINE message #-}


-- | Add public parameter
public ∷ Bool → Request f a Public
public = add "public" . boolToText
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
status = add "status" . \s -> case s of
  Attending → "0"
  Maybe     → "1"
  _         → "2"
{-# INLINE status #-}


-- | Add event parameter
event ∷ Int64 → Request f a Event
event = add "event" . T.toLazyText . T.decimal
{-# INLINE event #-}


-- | Add festivalsonly parameter
festivalsonly ∷ Bool → Request f a Festivals
festivalsonly = add "festivalsonly" . boolToText
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
distance = add "distance" . T.toLazyText . T.decimal
{-# INLINE distance #-}


-- | Add venue parameter
venue ∷ Int64 → Request f a Venue
venue = add "venue" . T.toLazyText . T.decimal
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
start = add "start" . T.toLazyText . T.decimal
{-# INLINE start #-}


-- | Add end parameter
end ∷ Int64 → Request f a End
end = add "end" . T.toLazyText . T.decimal
{-# INLINE end #-}


-- | Add startTimestamp parameter
startTimestamp ∷ Int64 → Request f a StartTimestamp
startTimestamp = add "startTimestamp" . T.toLazyText . T.decimal
{-# INLINE startTimestamp #-}


-- | Add endTimestamp parameter
endTimestamp ∷ Int64 → Request f a EndTimestamp
endTimestamp = add "endTimestamp" . T.toLazyText . T.decimal
{-# INLINE endTimestamp #-}


-- | Add from parameter
from ∷ Int64 → Request f a From
from = add "from" . T.toLazyText . T.decimal
{-# INLINE from #-}


-- | Add to parameter
to ∷ Int64 → Request f a To
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


boolToText ∷ Bool → Text
boolToText b = if b then "1" else "0"
{-# INLINE boolToText #-}
