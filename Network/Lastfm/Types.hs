{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Types where

import Control.Monad.Error (Error)
import Data.List (intercalate)

class Argument a where
  key :: a -> String
  value :: a -> String

(?<) :: Argument a => String -> a -> (String, String)
a ?< b = (a, value b)

(#) :: Argument a => a -> (String, String)
(#) x = (key x, value x)

newtype Method = Method {method :: String}
instance Argument Method where key = const "method"; value = method

newtype APIKey = APIKey String
newtype Autocorrect = Autocorrect Bool
newtype From = From Integer
newtype Group = Group String
newtype Language = Language String
newtype Limit = Limit Int
newtype Page = Page Int
newtype SessionKey = SessionKey String
newtype Tag = Tag String
newtype To = To Integer
instance Argument APIKey where key = const "api_key"; value (APIKey a) = a
instance Argument Autocorrect where key = const "autocorrect"; value (Autocorrect a) = value a
instance Argument From where key = const "from"; value (From a) = value a
instance Argument Group where key = const "group"; value (Group a) = a
instance Argument Language where key = const "lang"; value (Language a) = a
instance Argument Limit where key = const "limit"; value (Limit a) = value a
instance Argument Page where key = const "page"; value (Page a) = value a
instance Argument SessionKey where key = const "sk"; value (SessionKey a) = a
instance Argument Tag where key = const "tag"; value (Tag a) = a
instance Argument To where key = const "to"; value (To a) = value a

newtype Secret = Secret String deriving Show

newtype Album = Album String deriving (Show, Argument)
newtype AlbumArtist = AlbumArtist String deriving (Show, Argument)
newtype Artist = Artist String deriving (Show, Argument)
newtype AuthToken = AuthToken String deriving (Show, Argument)
newtype Bitrate = Bitrate Int deriving (Show, Argument)
newtype BuyLinks = BuyLinks Bool deriving (Show, Argument)
newtype ChosenByUser = ChosenByUser String deriving (Show, Argument)
newtype Context = Context String deriving (Show, Argument)
newtype Country = Country String deriving (Show, Argument)
newtype Description = Description String deriving (Show, Argument)
newtype Discovery = Discovery Bool deriving (Show, Argument)
newtype Distance = Distance Int deriving (Show, Argument)
newtype Duration = Duration Int deriving (Show, Argument)
newtype Event = Event Int deriving (Show, Argument)
newtype FestivalsOnly = FestivalsOnly Bool deriving (Show, Argument)
newtype Fingerprint = Fingerprint Integer deriving (Show, Argument)
newtype Latitude = Latitude String deriving (Show, Argument)
newtype Location = Location String deriving (Show, Argument)
newtype Longitude = Longitude String deriving (Show, Argument)
newtype Mbid = Mbid String deriving (Show, Argument)
newtype Message = Message String deriving (Show, Argument)
newtype Metro = Metro String deriving (Show, Argument)
newtype Multiplier = Multiplier Double deriving (Show, Argument)
newtype Name = Name String deriving (Show, Argument)
newtype Playlist = Playlist Int deriving (Show, Argument)
newtype Public = Public Bool deriving (Show, Argument)
newtype RecentTracks = RecentTracks Bool deriving (Show, Argument)
newtype Recipient = Recipient String deriving (Show, Argument)
newtype RTP = RTP Bool deriving (Show, Argument)
newtype Station = Station String deriving (Show, Argument)
newtype StreamId = StreamId String deriving (Show, Argument)
newtype TaggingType = TaggingType String deriving (Show, Argument)
newtype Timestamp = Timestamp Integer deriving (Show, Argument)
newtype Title = Title String deriving (Show, Argument)
newtype Token = Token String deriving (Show, Argument)
newtype Track = Track String deriving (Show, Argument)
newtype TrackNumber = TrackNumber String deriving (Show, Argument)
newtype User = User String deriving (Show, Argument)
newtype UseRecs = UseRecs Bool deriving (Show, Argument)
newtype Venue = Venue Int deriving (Show, Argument)

data Order = Popularity | DateAdded deriving Show

instance Argument Order where
  value Popularity = "popularity"
  value DateAdded  = "dateadded"

data Status = Yes | Maybe | No deriving Show

instance Argument Status where
  value Yes = "0"
  value Maybe = "1"
  value No = "2"

data Value = ValueUser User
           | ValueArtists [Artist]

instance Show Value where
  show (ValueUser _)    = "user"
  show (ValueArtists _) = "artists"

instance Argument Value where
  value (ValueUser u)     = value u
  value (ValueArtists as) = value as

data Period = Week | Quater | HalfYear | Year | Overall
              deriving (Show)

instance Argument Period where
  value Week     = "7day"
  value Quater   = "3month"
  value HalfYear = "6month"
  value Year     = "12month"
  value Overall  = "overall"

instance Argument Bool where
  value True = "1"
  value False = "0"
instance Argument Int where
  value = show
instance Argument Integer where
  value = show
instance Argument Double where
  value = show
instance Argument String where
  value = id
instance Argument a => Argument [a] where
  value = intercalate "," . map value
instance Argument a => Argument (Maybe a) where
  value (Just a) = value a
  value Nothing  = ""


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

