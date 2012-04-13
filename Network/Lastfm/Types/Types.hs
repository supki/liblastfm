{-# LANGUAGE OverloadedStrings #-}
module Network.Lastfm.Types.Types where

import Control.Applicative ((<$>), empty)
import Control.Monad (liftM)
import Data.Aeson ((.:), FromJSON, decode, parseJSON)
import Data.Maybe (fromJust)
import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

newtype Album = Album String
newtype AlbumArtist = AlbumArtist String
newtype APIKey = APIKey String
newtype Artist = Artist String
newtype AuthToken = AuthToken String
newtype ChosenByUser = ChosenByUser String
newtype Context = Context String
newtype Country = Country String
newtype Description = Description String
newtype Group = Group String
newtype Language = Language String
newtype Latitude = Latitude String
newtype Location = Location String
newtype Longitude = Longitude String
newtype Mbid = Mbid String
newtype Message = Message String
newtype Method = Method String
newtype Metro = Metro String
newtype Name = Name String
newtype Recipient = Recipient String
newtype SessionKey = SessionKey String
newtype Station = Station String
newtype StreamId = StreamId String
newtype Tag = Tag String
newtype TaggingType = TaggingType String
newtype Title = Title String
newtype Token = Token String
newtype Track = Track String
newtype User = User String
newtype Username = Username String
newtype Venuename = Venuename String

newtype Autocorrect = Autocorrect Bool
newtype BuyLinks = BuyLinks Bool
newtype Discovery = Discovery Bool
newtype FestivalsOnly = FestivalsOnly Bool
newtype Public = Public Bool
newtype RecentTracks = RecentTracks Bool
newtype RTP = RTP Bool
newtype UseRecs = UseRecs Bool

newtype Distance = Distance Int
newtype Duration = Duration Int
newtype Event = Event Int
newtype Limit = Limit Int
newtype Page = Page Int
newtype Playlist = Playlist Int
newtype TrackNumber = TrackNumber Int
newtype Venue = Venue Int

newtype End = End Integer
newtype EndTimestamp = EndTimestamp Integer
newtype Fingerprint = Fingerprint Integer
newtype From = From Integer
newtype Start = Start Integer
newtype StartTimestamp = StartTimestamp Integer
newtype Timestamp = Timestamp Integer
newtype To = To Integer

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

simple :: (FromJSON a, Monad m) => m String -> m a
simple = liftM (fromJust . decode . BL.pack)
