{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}
module Network.Lastfm.Types where

import Control.Monad.Error (Error)
import Data.List (intercalate)

newtype Secret = Secret String deriving Show

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

boolToString :: Bool -> String
boolToString True = "1"
boolToString False = "0"

newtype Autocorrect = Autocorrect Bool
newtype BuyLinks = BuyLinks Bool
newtype Discovery = Discovery Bool
newtype FestivalsOnly = FestivalsOnly Bool
newtype Public = Public Bool
newtype RecentTracks = RecentTracks Bool
newtype RTP = RTP Bool
newtype UseRecs = UseRecs Bool
instance Argument Autocorrect where key = const "autocorrect"; value (Autocorrect a) = boolToString a
instance Argument BuyLinks where key = const "buylinks"; value (BuyLinks a) = boolToString a
instance Argument Discovery where key = const "discovery"; value (Discovery a) = boolToString a
instance Argument FestivalsOnly where key = const "festivalsonly"; value (FestivalsOnly a) = boolToString a
instance Argument Public where key = const "public"; value (Public a) = boolToString a
instance Argument RecentTracks where key = const "recenttracks"; value (RecentTracks a) = boolToString a
instance Argument RTP where key = const "rtp"; value (RTP a) = boolToString a
instance Argument UseRecs where key = const "userecs"; value (UseRecs a) = boolToString a

newtype Distance = Distance Int
newtype Duration = Duration Int
newtype Event = Event Int
newtype Limit = Limit Int
newtype Page = Page Int
newtype Playlist = Playlist Int
newtype TrackNumber = TrackNumber Int
newtype Venue = Venue Int
instance Argument Distance where key = const "distance"; value (Distance a) = show a
instance Argument Duration where key = const "duration"; value (Duration a) = show a
instance Argument Event where key = const "event"; value (Event a) = show a
instance Argument Limit where key = const "limit"; value (Limit a) = show a
instance Argument Page where key = const "page"; value (Page a) = show a
instance Argument Playlist where key = const "playlistID"; value (Playlist a) = show a
instance Argument TrackNumber where key = const "tracknumber"; value (TrackNumber a) = show a
instance Argument Venue where key = const "venue"; value (Venue a) = show a

newtype End = End Integer
newtype EndTimestamp = EndTimestamp Integer
newtype Fingerprint = Fingerprint Integer
newtype From = From Integer
newtype Start = Start Integer
newtype StartTimestamp = StartTimestamp Integer
newtype Timestamp = Timestamp Integer
newtype To = To Integer
instance Argument End where key = const "end"; value (End a) = show a
instance Argument EndTimestamp where key = const "endTimestamp"; value (EndTimestamp a) = show a
instance Argument Fingerprint where key = const "fingerprintid"; value (Fingerprint a) = show a
instance Argument From where key = const "from"; value (From a) = show a
instance Argument Start where key = const "start"; value (Start a) = show a
instance Argument StartTimestamp where key = const "startTimestamp"; value (StartTimestamp a) = show a
instance Argument Timestamp where key = const "timestamp"; value (Timestamp a) = show a
instance Argument To where key = const "to"; value (To a) = show a

data Bitrate = B64 | B128

instance Argument Bitrate where
  key = const "bitrate"
  value B64 = "64"
  value B128 = "128"

data Multiplier = M1 | M2
instance Argument Multiplier where
  key = const "speed_multiplier"
  value M1 = "1.0"
  value M2 = "2.0"

data Order = Popularity | DateAdded
instance Argument Order where
  key = const "order"
  value Popularity = "popularity"
  value DateAdded  = "dateadded"

data Status = Yes | Maybe | No
instance Argument Status where
  key = const "status"
  value Yes = "0"
  value Maybe = "1"
  value No = "2"

data Value = ValueUser User
           | ValueArtists [Artist]
instance Show Value where
  show (ValueUser _)    = "user"
  show (ValueArtists _) = "artists"
instance Argument Value where
  key = const "value"
  value (ValueUser u)     = value u
  value (ValueArtists as) = value as

data Period = Week | Quater | HalfYear | Year | Overall
instance Argument Period where
  key = const "period"
  value Week     = "7day"
  value Quater   = "3month"
  value HalfYear = "6month"
  value Year     = "12month"
  value Overall  = "overall"

data APIError
  = DoesntExist
  | InvalidService
  | InvalidMethod
  | AuthenticationFailed
  | InvalidFormat
  | InvalidParameters
  | InvalidResource
  | OperationFailed
  | InvalidSessionKey
  | InvalidAPIKey
  | ServiceOffline
  | SubscribersOnly
  | InvalidMethodSignature
  | TokenHasNotAuthorized
  | NotForStreaming
  | TemporaryUnavailable
  | LoginRequired
  | TrialExpired
  | DoesntExistAgain
  | NotEnoughContent
  | NotEnoughMembers
  | NotEnoughFans
  | NotEnoughNeighbours
  | NoPeakRadio
  | RadioNotFound
  | SuspendedAPIKey
  | Deprecated
  | RateLimitExceeded
    deriving (Enum)

instance Show APIError where
  show DoesntExist = "DoesntExist: This error does not exist"
  show InvalidService = "InvalidService: This service does not exist"
  show InvalidMethod = "InvalidMethod: No method with that name in this package"
  show AuthenticationFailed = "AuthenticationFailed: You do not have permissions to access the service"
  show InvalidFormat = "InvalidFormat: This service doesn't exist in that format"
  show InvalidParameters = "InvalidParameters: Your request is missing a required parameter"
  show InvalidResource = "InvalidResource: Invalid resource specified"
  show OperationFailed = "OperationFailed: Something else went wrong"
  show InvalidSessionKey = "InvalidSessionKey: Please re-authenticate"
  show InvalidAPIKey = "InvalidAPIKey: You must be granted a valid key by last.fm"
  show ServiceOffline = "ServiceOffline: This service is temporarily offline. Try again later."
  show SubscribersOnly  = "SubscribersOnly : This station is only available to paid last.fm subscribers"
  show InvalidMethodSignature = "InvalidMethodSignature: Invalid method signature supplied"
  show TokenHasNotAuthorized = "TokenHasNotAuthorized: This token has not been authorized"
  show NotForStreaming = "NotForStreaming: This item is not available for streaming."
  show TemporaryUnavailable = "TemporaryUnavailable: The service is temporarily unavailable, please try again."
  show LoginRequired = "LoginRequired: Login: User requires to be logged in"
  show TrialExpired = "TrialExpired: This user has no free radio plays left. Subscription required."
  show DoesntExistAgain = "DoesntExistAgain: This error does not exist"
  show NotEnoughContent = "NotEnoughContent: There is not enough content to play this station"
  show NotEnoughMembers = "NotEnoughMembers: This group does not have enough members for radio"
  show NotEnoughFans = "NotEnoughFans: This artist does not have enough fans for for radio"
  show NotEnoughNeighbours = "NotEnoughNeighbours: There are not enough neighbours for radio"
  show NoPeakRadio = "NoPeakRadio: This user is not allowed to listen to radio during peak usage"
  show RadioNotFound = "RadioNotFound: Radio station not found"
  show SuspendedAPIKey = "SuspendedAPIKey: Access for your account has been suspended, please contact Last.fm"
  show Deprecated = "Deprecated: This type of request is no longer supported"
  show RateLimitExceeded = "RateLimitExceeded: Your IP has made too many requests in a short period"

-- Various Lastfm errors.
data LastfmError
  = LastfmAPIError APIError -- ^ Internal Lastfm errors
  | WrapperCallError String String -- ^ Wrapper errors
    deriving Show

instance Error LastfmError

