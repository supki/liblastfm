{-# LANGUAGE TemplateHaskell #-}
module Network.Lastfm.Types.Argument where

import Data.List (intercalate)

import Network.Lastfm.Types.TH
import Network.Lastfm.Types.Types

(#) :: Argument a => a -> (String, String)
(#) x = (key x, value x)

class Argument a where
  key :: a -> String
  value :: a -> String

instance Argument a => Argument (Maybe a) where key = maybe "" key; value = maybe "" value
instance Argument a => Argument [a] where
  key (a:_) = key a ++ "s"
  key [] = ""
  value = intercalate "," . map value

boolToString :: Bool -> String
boolToString True = "1"
boolToString False = "0"

--instance Argument $first where key = const $second; value ($first a) = $func a
$(instances "id"
  [ ("Album","album")
  , ("AlbumArtist", "albumartist")
  , ("APIKey", "api_key")
  , ("Artist", "artist")
  , ("AuthToken", "authToken")
  , ("ChosenByUser", "chosenByUser")
  , ("Context", "context")
  , ("Country", "country")
  , ("Description", "description")
  , ("Group", "group")
  , ("Language", "lang")
  , ("Latitude", "lat")
  , ("Location", "location")
  , ("Longitude", "long")
  , ("Mbid", "mbid")
  , ("Message", "message")
  , ("Method", "method")
  , ("Metro", "metro")
  , ("Name", "name")
  , ("Recipient", "recipient")
  , ("SessionKey", "sk")
  , ("Station", "station")
  , ("StreamId", "streamId")
  , ("Tag", "tag")
  , ("TaggingType", "taggingtype")
  , ("Title", "title")
  , ("Token", "token")
  , ("Track", "track")
  , ("User", "user")
  , ("Username", "username")
  , ("Venuename", "venue")
  ])

$( instances "boolToString"
  [ ("Autocorrect", "autocorrect")
  , ("BuyLinks", "buylinks")
  , ("Discovery", "discovery")
  , ("FestivalsOnly", "festivalsonly")
  , ("Public", "public")
  , ("RecentTracks", "recenttracks")
  , ("RTP", "rtp")
  , ("UseRecs", "userecs")
  ])

$( instances "show"
  [ ("Distance", "distance")
  , ("Duration", "duration")
  , ("Event", "event")
  , ("Limit", "limit")
  , ("Page", "page")
  , ("Playlist", "playlistID")
  , ("TrackNumber", "tracknumber")
  , ("Venue", "venue")
  , ("End", "end")
  , ("EndTimestamp", "endTimestamp")
  , ("Fingerprint", "fingerprintid")
  , ("From", "from")
  , ("Start", "start")
  , ("StartTimestamp", "startTimestamp")
  , ("Timestamp", "timestamp")
  , ("To", "to")
  ])

instance Argument Bitrate where
  key = const "bitrate"
  value B64 = "64"
  value B128 = "128"

instance Argument Multiplier where
  key = const "speed_multiplier"
  value M1 = "1.0"
  value M2 = "2.0"

instance Argument Order where
  key = const "order"
  value Popularity = "popularity"
  value DateAdded  = "dateadded"

instance Argument Status where
  key = const "status"
  value Yes = "0"
  value Maybe = "1"
  value No = "2"

instance Argument Value where
  key = const "value"
  value (ValueUser u)     = value u
  value (ValueArtists as) = value as

instance Argument Period where
  key = const "period"
  value Week     = "7day"
  value Quater   = "3month"
  value HalfYear = "6month"
  value Year     = "12month"
  value Overall  = "overall"
