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
  , ChosenByUser, chosenByUser, Context, context, StreamId, streamId
  , RecentTracks, recentTracks
  , Recipient, recipient, Username, username, User, user, Password, password
  , Public, public, Message, message, Page, page, Limit, limit
  , TaggingType, taggingType, UseRecs, useRecs, Venue, venue, VenueName, venueName
  , type', Value', value
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

class ArtistTrackOrMBID a

instance ArtistTrackOrMBID MBID
instance ArtistTrackOrMBID (Artist, Track)

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
data Value'
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
value ∷ Int64 → Text → Request f a Value'
value n = add ("value" <> T.toLazyText (T.decimal n))
{-# INLINE value #-}


-- | Add track parameter
track ∷ Text → Request f a Track
track = add "track"
{-# INLINE track #-}


-- | Add timestamp parameter
timestamp ∷ Int64 → Request f a Timestamp
timestamp = add "timestamp" . T.toLazyText . T.decimal
{-# INLINE timestamp #-}


-- | Add fingerprint parameter
fingerprint ∷ Int64 → Request f a Fingerprint
fingerprint = add "fingerprintid" . T.toLazyText . T.decimal
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
streamId = add "streamId" . T.toLazyText . T.decimal
{-# INLINE streamId #-}


-- | Add duration parameter
duration ∷ Int64 → Request f a Duration
duration = add "duration" . T.toLazyText . T.decimal
{-# INLINE duration #-}


-- | Add trackNumber parameter
trackNumber ∷ Int64 → Request f a TrackNumber
trackNumber = add "trackNumber" . T.toLazyText . T.decimal
{-# INLINE trackNumber #-}


-- | Add chosenByUser parameter
chosenByUser ∷ Bool → Request f a ChosenByUser
chosenByUser = add "chosenByUser" . boolToText
{-# INLINE chosenByUser #-}


-- | Add taggingType parameter
taggingType ∷ Text → Request f a TaggingType
taggingType = add "taggingtype"
{-# INLINE taggingType #-}


-- | Add recentTracks parameter
recentTracks ∷ Bool → Request f a RecentTracks
recentTracks = add "recentTracks" . boolToText
{-# INLINE recentTracks #-}


-- | Add useRecs parameter
useRecs ∷ Bool → Request f a UseRecs
useRecs = add "useRecs" . boolToText
{-# INLINE useRecs #-}


-- | Add group parameter
group ∷ Text → Request f a Group
group = add "group"
{-# INLINE group #-}


boolToText ∷ Bool → Text
boolToText b = if b then "1" else "0"
{-# INLINE boolToText #-}
