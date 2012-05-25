{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Lastfm.Types.Types where

import Control.Applicative ((<$>), empty)
import Control.Monad (liftM)
import Data.Aeson ((.:), FromJSON, decode, parseJSON)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import qualified Data.Aeson

import Network.Lastfm.Types.TH

$(newtypes "String" ["Album", "AlbumArtist", "APIKey", "Artist", "AuthToken", "Context", "Country", "Description", "Group", "Language", "Latitude", "Location", "Longitude", "Mbid", "Message", "Method", "Metro", "Name", "Recipient", "SessionKey", "Station", "StreamId", "Tag", "TaggingType", "Title", "Token", "Track", "User", "Username", "Venuename", "ChosenByUser"])

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
  show (ValueUser _)    = "user"
  show (ValueArtists _) = "artists"

instance FromJSON Token where
  parseJSON (Data.Aeson.Object v) = Token <$> v .: "token"
  parseJSON _ = empty
instance FromJSON SessionKey where
  parseJSON (Data.Aeson.Object v) = SessionKey <$> ((v .: "session") >>= (.: "key"))
  parseJSON _ = empty

simple ∷ (FromJSON a, Monad m) ⇒ m ByteString → m a
simple = liftM (fromJust . decode)
