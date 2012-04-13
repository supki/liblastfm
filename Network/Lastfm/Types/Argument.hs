module Network.Lastfm.Types.Argument where

import Data.List (intercalate)
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

instance Argument Album where key = const "album"; value (Album a) = a
instance Argument AlbumArtist where key = const "albumartist"; value (AlbumArtist a) = a
instance Argument APIKey where key = const "api_key"; value (APIKey a) = a
instance Argument Artist where key = const "artist"; value (Artist a) = a
instance Argument AuthToken where key = const "authToken"; value (AuthToken a) = a
instance Argument ChosenByUser where key = const "chosenByUser"; value (ChosenByUser a) = a
instance Argument Context where key = const "context"; value (Context a) = a
instance Argument Country where key = const "country"; value (Country a) = a
instance Argument Description where key = const "description"; value (Description a) = a
instance Argument Group where key = const "group"; value (Group a) = a
instance Argument Language where key = const "lang"; value (Language a) = a
instance Argument Latitude where key = const "lat"; value (Latitude a) = a
instance Argument Location where key = const "location"; value (Location a) = a
instance Argument Longitude where key = const "long"; value (Longitude a) = a
instance Argument Mbid where key = const "mbid"; value (Mbid a) = a
instance Argument Message where key = const "message"; value (Message a) = a
instance Argument Method where key = const "method"; value (Method a) = a
instance Argument Metro where key = const "metro"; value (Metro a) = a
instance Argument Name where key = const "name"; value (Name a) = a
instance Argument Recipient where key = const "recipient"; value (Recipient a) = a
instance Argument SessionKey where key = const "sk"; value (SessionKey a) = a
instance Argument Station where key = const "station"; value (Station a) = a
instance Argument StreamId where key = const "streamId"; value (StreamId a) = a
instance Argument Tag where key = const "tag"; value (Tag a) = a
instance Argument TaggingType where key = const "taggingtype"; value (TaggingType a) = a
instance Argument Title where key = const "title"; value (Title a) = a
instance Argument Token where key = const "token"; value (Token a) = a
instance Argument Track where key = const "track"; value (Track a) = a
instance Argument User where key = const "user"; value (User a) = a
instance Argument Username where key = const "username"; value (Username a) = a
instance Argument Venuename where key = const "venue"; value (Venuename a) = a

boolToString :: Bool -> String
boolToString True = "1"
boolToString False = "0"

instance Argument Autocorrect where key = const "autocorrect"; value (Autocorrect a) = boolToString a
instance Argument BuyLinks where key = const "buylinks"; value (BuyLinks a) = boolToString a
instance Argument Discovery where key = const "discovery"; value (Discovery a) = boolToString a
instance Argument FestivalsOnly where key = const "festivalsonly"; value (FestivalsOnly a) = boolToString a
instance Argument Public where key = const "public"; value (Public a) = boolToString a
instance Argument RecentTracks where key = const "recenttracks"; value (RecentTracks a) = boolToString a
instance Argument RTP where key = const "rtp"; value (RTP a) = boolToString a
instance Argument UseRecs where key = const "userecs"; value (UseRecs a) = boolToString a

instance Argument Distance where key = const "distance"; value (Distance a) = show a
instance Argument Duration where key = const "duration"; value (Duration a) = show a
instance Argument Event where key = const "event"; value (Event a) = show a
instance Argument Limit where key = const "limit"; value (Limit a) = show a
instance Argument Page where key = const "page"; value (Page a) = show a
instance Argument Playlist where key = const "playlistID"; value (Playlist a) = show a
instance Argument TrackNumber where key = const "tracknumber"; value (TrackNumber a) = show a
instance Argument Venue where key = const "venue"; value (Venue a) = show a

instance Argument End where key = const "end"; value (End a) = show a
instance Argument EndTimestamp where key = const "endTimestamp"; value (EndTimestamp a) = show a
instance Argument Fingerprint where key = const "fingerprintid"; value (Fingerprint a) = show a
instance Argument From where key = const "from"; value (From a) = show a
instance Argument Start where key = const "start"; value (Start a) = show a
instance Argument StartTimestamp where key = const "startTimestamp"; value (StartTimestamp a) = show a
instance Argument Timestamp where key = const "timestamp"; value (Timestamp a) = show a
instance Argument To where key = const "to"; value (To a) = show a

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
