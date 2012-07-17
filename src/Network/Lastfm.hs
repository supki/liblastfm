{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Lastfm
  ( Lastfm, Response, LastfmError(..)
  , Secret(..)
  , Argument(..)
  , Album(..)
  , AlbumArtist(..)
  , APIKey(..)
  , Artist(..)
  , AuthToken(..)
  , Autocorrect(..)
  , Bitrate(..)
  , ChosenByUser(..)
  , Context(..)
  , Country(..)
  , Description(..)
  , Group(..)
  , Language(..)
  , Latitude(..)
  , Location(..)
  , Longitude(..)
  , Mbid(..)
  , Message(..)
  , Method(..)
  , Metro(..)
  , Multiplier(..)
  , Name(..)
  , Order(..)
  , Period(..)
  , Recipient(..)
  , SessionKey(..)
  , Status(..)
  , Station(..)
  , StreamId(..)
  , Tag(..)
  , TaggingType(..)
  , Title(..)
  , Token(..)
  , Track(..)
  , User(..)
  , Username(..)
  , Value(..)
  , Venuename(..)
  , BuyLinks(..)
  , Discovery(..)
  , FestivalsOnly(..)
  , Public(..)
  , RecentTracks(..)
  , RTP(..)
  , UseRecs(..)
  , Distance(..)
  , Duration(..)
  , Event(..)
  , Limit(..)
  , Page(..)
  , Playlist(..)
  , TrackNumber(..)
  , Venue(..)
  , End(..)
  , EndTimestamp(..)
  , Fingerprint(..)
  , From(..)
  , Start(..)
  , StartTimestamp(..)
  , Timestamp(..)
  , To(..)
  , simple
  ) where

import Control.Applicative ((<$>), empty)
import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromJust)

import Data.Aeson ((.:), FromJSON, decode, parseJSON)
import qualified Data.Aeson
import Data.ByteString.Lazy.Char8 (ByteString)

import Network.Lastfm.Error
import Network.Lastfm.TH


type Lastfm a = IO (Either LastfmError a)
type Response = ByteString


newtype Secret = Secret String


$(newtypes "String" ["Album", "AlbumArtist", "APIKey", "Artist", "AuthToken",
  "Context", "Country", "Description", "Group", "Language", "Latitude",
  "Location", "Longitude", "Mbid", "Message", "Method", "Metro", "Name",
  "Recipient", "SessionKey", "Station", "StreamId", "Tag", "TaggingType",
  "Title", "Token", "Track", "User", "Username", "Venuename", "ChosenByUser"])
$(newtypes "Bool" ["Autocorrect", "BuyLinks", "Discovery", "FestivalsOnly", "Public", "RecentTracks", "RTP", "UseRecs"])
$(newtypes "Int" ["Distance", "Duration", "Event", "Limit", "Page", "Playlist", "TrackNumber", "Venue"])
$(newtypes "Integer" ["End", "EndTimestamp", "Fingerprint", "From", "Start", "StartTimestamp", "Timestamp", "To"])


data Bitrate = B64 | B128
data Multiplier = M1 | M2
data Order = Popularity | DateAdded
data Status = Yes | Maybe | No
data Value = ValueUser User
           | ValueArtists [Artist]
data Period = Week | Quater | HalfYear | Year | Overall


instance Show Value where
  show (ValueUser _) = "user"
  show (ValueArtists _) = "artists"


instance FromJSON Token where
  parseJSON (Data.Aeson.Object v) = Token <$> v .: "token"
  parseJSON _ = empty


instance FromJSON SessionKey where
  parseJSON (Data.Aeson.Object v) = SessionKey <$> ((v .: "session") >>= (.: "key"))
  parseJSON _ = empty


simple ∷ (FromJSON a, Monad m) ⇒ m ByteString → m a
simple = liftM (fromJust . decode)


class Argument a where
  key ∷ a → String
  value ∷ a → String


instance Argument a ⇒ Argument (Maybe a) where key = maybe "" key; value = maybe "" value
instance Argument a ⇒ Argument [a] where
  key (a:_) = key a ++ "s"
  key [] = ""
  value = intercalate "," . map value


boolToString ∷ Bool → String
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
  value DateAdded = "dateadded"


instance Argument Status where
  key = const "status"
  value Yes = "0"
  value Maybe = "1"
  value No = "2"


instance Argument Value where
  key = const "value"
  value (ValueUser u) = value u
  value (ValueArtists as) = value as


instance Argument Period where
  key = const "period"
  value Week = "7day"
  value Quater = "3month"
  value HalfYear = "6month"
  value Year = "12month"
  value Overall = "overall"
