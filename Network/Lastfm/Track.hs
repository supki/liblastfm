{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Track
  ( AlbumArtist(..), ChosenByUser(..), Context(..), Duration(..)
  , Limit(..), Mbid(..), Message(..), Page(..), Public(..), Recipient(..)
  , StreamId(..), Tag(..), Timestamp(..), Track(..), TrackNumber(..)
  , addTags, ban, getBuyLinks, getCorrection, getFingerprintMetadata
  , getInfo, getShouts, getSimilar, getTags, getTopFans, getTopTags
  , love, removeTag, scrobble, search, share, unban, unlove, updateNowPlaying
  ) where

import Data.Maybe (isJust)
import Prelude hiding (either)

import Network.Lastfm.Album (Album)
import Network.Lastfm.Artist (Artist)
import Network.Lastfm.Auth (APIKey, SessionKey)
import Network.Lastfm.Core

newtype AlbumArtist = AlbumArtist String deriving (Show, LastfmValue)
newtype Autocorrect = Autocorrect Bool deriving (Show, LastfmValue)
newtype ChosenByUser = ChosenByUser String deriving (Show, LastfmValue)
newtype Context = Context String deriving (Show, LastfmValue)
newtype Country = Country String deriving (Show, LastfmValue)
newtype Duration = Duration String deriving (Show, LastfmValue)
newtype Fingerprint = Fingerprint String deriving (Show, LastfmValue)
newtype Limit = Limit Int deriving (Show, LastfmValue)
newtype Mbid = Mbid String deriving (Show, LastfmValue)
newtype Message = Message String deriving (Show, LastfmValue)
newtype Page = Page String deriving (Show, LastfmValue)
newtype Public = Public Bool deriving (Show, LastfmValue)
newtype Recipient = Recipient String deriving (Show, LastfmValue)
newtype StreamId = StreamId String deriving (Show, LastfmValue)
newtype Tag = Tag String deriving (Show, LastfmValue)
newtype Timestamp = Timestamp String deriving (Show, LastfmValue)
newtype Track = Track String deriving (Show, LastfmValue)
newtype TrackNumber = TrackNumber String deriving (Show, LastfmValue)
newtype Username = Username String deriving (Show, LastfmValue)

addTags :: Artist -> Track -> [Tag] -> APIKey -> SessionKey -> Lastfm ()
addTags artist track tags apiKey sessionKey
  | null tags        = error "Track.addTags: empty tag list."
  | length tags > 10 = error "Track.addTags: tag list length has exceeded maximum."
  | otherwise        = callAPI_ "track.addTags"
    [ "artist" ?< artist
    , "track" ?< track
    , "tags" ?< tags
    , "api_key" ?< apiKey
    , "sk" ?< sessionKey
    ]

ban :: Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
ban track artist apiKey sessionKey = callAPI_ "track.ban"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getBuyLinks :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Country -> APIKey -> Lastfm Response
getBuyLinks a mbid autocorrect country apiKey = callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "country" ?< country
  , "api_key" ?< apiKey
  ]
  where method = "track.getBuyLinks"
        parameters = either method a mbid

getCorrection :: Artist -> Track -> APIKey -> Lastfm Response
getCorrection artist track apiKey = callAPI "track.getCorrection"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  ]

getFingerprintMetadata :: Fingerprint -> APIKey -> Lastfm Response
getFingerprintMetadata fingerprint apiKey = callAPI "track.getFingerprintMetadata"
  [ "fingerprintid" ?< fingerprint
  , "api_key" ?< apiKey
  ]

getInfo :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Username -> APIKey -> Lastfm Response
getInfo a mbid autocorrect username apiKey = callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "username" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "track.getInfo"
        parameters = either method a mbid

getShouts :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Limit -> Maybe Autocorrect -> Maybe Page -> APIKey -> Lastfm Response
getShouts a mbid limit autocorrect page apiKey = callAPI method $ parameters ++
  [ "limit" ?< limit
  , "autocorrect" ?< autocorrect
  , "page" ?< page
  , "api_key" ?< apiKey
  ]
  where method = "track.getShouts"
        parameters = either method a mbid

getSimilar :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Limit -> APIKey -> Lastfm Response
getSimilar a mbid autocorrect limit apiKey = callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "track.getSimilar"
        parameters = either method a mbid

getTags :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Username -> APIKey -> Lastfm Response
getTags a mbid autocorrect username apiKey = callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "user" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "track.getTags"
        parameters = either method a mbid

getTopFans :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopFans a mbid autocorrect apiKey = callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "track.getTopFans"
        parameters = either method a mbid

getTopTags :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a mbid autocorrect apiKey = callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "track.getTopTags"
        parameters = either method a mbid
love :: Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
love track artist apiKey sessionKey = callAPI_ "track.love"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

removeTag :: Artist -> Track -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist track tag apiKey sessionKey = callAPI_ "track.removeTag"
  [ "artist" ?< artist
  , "track" ?< track
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

scrobble :: [ ( Timestamp, Maybe Album, Track, Artist, Maybe AlbumArtist
           , Maybe Duration, Maybe StreamId, Maybe ChosenByUser
           , Maybe Context, Maybe TrackNumber, Maybe Mbid ) ]
         -> APIKey
         -> SessionKey
         -> Lastfm ()
scrobble xs apiKey sessionKey = mapM_ scrobbleTrack xs
  where scrobbleTrack (timestamp, album, track, artist, albumArtist, duration, streamId, chosenByUser, context, trackNumber, mbid) = callAPI_ "track.scrobble"
          [ "timestamp" ?< timestamp
          , "track" ?< track
          , "artist" ?< artist
          , "api_key" ?< apiKey
          , "sk" ?< sessionKey
          , "album" ?< album
          , "albumArtist" ?< albumArtist
          , "duration" ?< duration
          , "streamId" ?< streamId
          , "chosenByUser" ?< chosenByUser
          , "context" ?< context
          , "trackNumber" ?< trackNumber
          , "mbid" ?< mbid
          ]

search :: Maybe Limit -> Maybe Page -> Track -> Maybe Artist -> APIKey -> Lastfm Response
search limit page track artist apiKey = callAPI "track.search"
  [ "track" ?< track
  , "api_key" ?< apiKey
  , "limit" ?< limit
  , "page" ?< page
  , "artist" ?< artist
  ]

share :: Artist -> Track -> Maybe Public -> Maybe Message -> [Recipient] -> APIKey -> SessionKey -> Lastfm ()
share artist track public message recipients apiKey sessionKey
  | null recipients        = error "track.share: empty recipient list."
  | length recipients > 10 = error "track.share: recipient list length has exceeded maximum."
  | otherwise              = callAPI_ "track.share"
    [ "artist" ?< artist
    , "track" ?< track
    , "recipient" ?< recipients
    , "api_key" ?< apiKey
    , "sk" ?< sessionKey
    , "public" ?< public
    , "message" ?< message
    ]

unban :: Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
unban track artist apiKey sessionKey = callAPI_ "track.unban"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

unlove :: Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
unlove track artist apiKey sessionKey = callAPI_ "track.unlove"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

updateNowPlaying :: Track
                 -> Artist
                 -> Maybe Album
                 -> Maybe AlbumArtist
                 -> Maybe Context
                 -> Maybe TrackNumber
                 -> Maybe Mbid
                 -> Maybe Duration
                 -> APIKey
                 -> SessionKey
                 -> Lastfm ()
updateNowPlaying track artist album albumArtist context trackNumber mbid duration apiKey sessionKey = callAPI_ "track.updateNowPlaying"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  , "album" ?< album
  , "albumArtist" ?< albumArtist
  , "context" ?< context
  , "trackNumber" ?< trackNumber
  , "mbid" ?< mbid
  , "duration" ?< duration
  ]

either method a mbid
  | isJust mbid = [ "mbid" ?< mbid ]
  | otherwise   = case a of
                    Just (artist, track) -> [ "artist" ?< artist, "track" ?< track ]
                    Nothing              -> error $ method ++ ": no mbid nor (artist, track) are specified."
