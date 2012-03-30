{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving, DeriveDataTypeable  #-}
module Network.Lastfm.Types where

import Control.Exception (Exception)
import Data.List (intercalate)
import Data.Typeable (Typeable)

newtype Secret = Secret String deriving Show

newtype Album = Album String deriving (Show, LastfmValue)
newtype AlbumArtist = AlbumArtist String deriving (Show, LastfmValue)
newtype APIKey = APIKey String deriving (Show, LastfmValue)
newtype Artist = Artist String deriving (Show, LastfmValue)
newtype AuthToken = AuthToken String deriving (Show, LastfmValue)
newtype Autocorrect = Autocorrect Bool deriving (Show, LastfmValue)
newtype Bitrate = Bitrate Int deriving (Show, LastfmValue)
newtype BuyLinks = BuyLinks Bool deriving (Show, LastfmValue)
newtype ChosenByUser = ChosenByUser String deriving (Show, LastfmValue)
newtype Context = Context String deriving (Show, LastfmValue)
newtype Country = Country String deriving (Show, LastfmValue)
newtype Description = Description String deriving (Show, LastfmValue)
newtype Discovery = Discovery Bool deriving (Show, LastfmValue)
newtype Distance = Distance Int deriving (Show, LastfmValue)
newtype Duration = Duration Int deriving (Show, LastfmValue)
newtype Event = Event Int deriving (Show, LastfmValue)
newtype FestivalsOnly = FestivalsOnly Bool deriving (Show, LastfmValue)
newtype Fingerprint = Fingerprint Integer deriving (Show, LastfmValue)
newtype From = From Integer deriving (Show, LastfmValue)
newtype Group = Group String deriving (Show, LastfmValue)
newtype Language = Language String deriving (Show, LastfmValue)
newtype Latitude = Latitude String deriving (Show, LastfmValue)
newtype Limit = Limit Int deriving (Show, LastfmValue)
newtype Location = Location String deriving (Show, LastfmValue)
newtype Longitude = Longitude String deriving (Show, LastfmValue)
newtype Mbid = Mbid String deriving (Show, LastfmValue)
newtype Message = Message String deriving (Show, LastfmValue)
newtype Metro = Metro String deriving (Show, LastfmValue)
newtype Multiplier = Multiplier Double deriving (Show, LastfmValue)
newtype Name = Name String deriving (Show, LastfmValue)
newtype Page = Page Int deriving (Show, LastfmValue)
newtype Playlist = Playlist Int deriving (Show, LastfmValue)
newtype Public = Public Bool deriving (Show, LastfmValue)
newtype RecentTracks = RecentTracks Bool deriving (Show, LastfmValue)
newtype Recipient = Recipient String deriving (Show, LastfmValue)
newtype RTP = RTP Bool deriving (Show, LastfmValue)
newtype SessionKey = SessionKey String deriving (Show, LastfmValue)
newtype Station = Station String deriving (Show, LastfmValue)
newtype StreamId = StreamId String deriving (Show, LastfmValue)
newtype Tag = Tag String deriving (Show, LastfmValue)
newtype TaggingType = TaggingType String deriving (Show, LastfmValue)
newtype Timestamp = Timestamp Integer deriving (Show, LastfmValue)
newtype Title = Title String deriving (Show, LastfmValue)
newtype To = To Integer deriving (Show, LastfmValue)
newtype Token = Token String deriving (Show, LastfmValue)
newtype Track = Track String deriving (Show, LastfmValue)
newtype TrackNumber = TrackNumber String deriving (Show, LastfmValue)
newtype User = User String deriving (Show, LastfmValue)
newtype UseRecs = UseRecs Bool deriving (Show, LastfmValue)
newtype Venue = Venue Int deriving (Show, LastfmValue)

data Order = Popularity | DateAdded deriving Show

instance LastfmValue Order where
  unpack Popularity = "popularity"
  unpack DateAdded  = "dateadded"

data Status = Yes | Maybe | No deriving Show

instance LastfmValue Status where
  unpack Yes = "0"
  unpack Maybe = "1"
  unpack No = "2"

class LastfmValue a where
  unpack :: a -> String

data Value = ValueUser User
           | ValueArtists [Artist]

instance Show Value where
  show (ValueUser _)    = "user"
  show (ValueArtists _) = "artists"

instance LastfmValue Value where
  unpack (ValueUser u)     = unpack u
  unpack (ValueArtists as) = unpack as

data Period = Week | Quater | HalfYear | Year | Overall
              deriving (Show)

instance LastfmValue Period where
  unpack Week     = "7day"
  unpack Quater   = "3month"
  unpack HalfYear = "6month"
  unpack Year     = "12month"
  unpack Overall  = "overall"

instance LastfmValue Bool where
  unpack True = "1"
  unpack False = "0"
instance LastfmValue Int where
  unpack = show
instance LastfmValue Integer where
  unpack = show
instance LastfmValue Double where
  unpack = show
instance LastfmValue String where
  unpack = id
instance LastfmValue a => LastfmValue [a] where
  unpack = intercalate "," . map unpack
instance LastfmValue a => LastfmValue (Maybe a) where
  unpack (Just a) = unpack a
  unpack Nothing  = ""

(?<) :: LastfmValue a => String -> a -> (String, String)
a ?< b = (a, unpack b)

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
    deriving (Show, Typeable)

instance Exception LastfmError

