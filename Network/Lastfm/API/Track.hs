module Network.Lastfm.API.Track
  ( addTags, ban, getBuyLinks, getCorrection, getFingerprintMetadata
  , getInfo, getShouts, getSimilar, getTags, getTopFans, getTopTags
  , love, removeTag, scrobble, search, share, unban, unlove, updateNowPlaying
  ) where

import Control.Exception (throw)
import Data.Maybe (isJust)
import Prelude hiding (either)

import Network.Lastfm.Response
import Network.Lastfm.Types ( (?<), Album, AlbumArtist, APIKey, Artist, Autocorrect, ChosenByUser, Context, Country
                            , Duration, Fingerprint, Limit, Mbid, Message, Page, Public, Recipient, SessionKey
                            , StreamId, Tag, Timestamp, Track, TrackNumber, User
                            )

addTags :: Artist -> Track -> [Tag] -> APIKey -> SessionKey -> Lastfm ()
addTags artist track tags apiKey sessionKey = dispatch go
  where go
          | null tags        = throw $ WrapperCallError method "empty tag list."
          | length tags > 10 = throw $ WrapperCallError method "tag list length has exceeded maximum."
          | otherwise        = callAPI_ method
            [ "artist" ?< artist
            , "track" ?< track
            , "tags" ?< tags
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "track.addTags"

ban :: Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
ban artist track apiKey sessionKey = dispatch $ callAPI_ "track.ban"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getBuyLinks :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Country -> APIKey -> Lastfm Response
getBuyLinks a mbid autocorrect country apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "country" ?< country
  , "api_key" ?< apiKey
  ]
  where method = "track.getBuyLinks"
        parameters = either method a mbid

getCorrection :: Artist -> Track -> APIKey -> Lastfm Response
getCorrection artist track apiKey = dispatch $ callAPI "track.getCorrection"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  ]

getFingerprintMetadata :: Fingerprint -> APIKey -> Lastfm Response
getFingerprintMetadata fingerprint apiKey = dispatch $ callAPI "track.getFingerprintMetadata"
  [ "fingerprintid" ?< fingerprint
  , "api_key" ?< apiKey
  ]

getInfo :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe User -> APIKey -> Lastfm Response
getInfo a mbid autocorrect username apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "username" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "track.getInfo"
        parameters = either method a mbid

getShouts :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts a mbid autocorrect page limit apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "track.getShouts"
        parameters = either method a mbid

getSimilar :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Limit -> APIKey -> Lastfm Response
getSimilar a mbid autocorrect limit apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "track.getSimilar"
        parameters = either method a mbid

getTags :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> Maybe User -> APIKey -> Lastfm Response
getTags a mbid autocorrect username apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "user" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "track.getTags"
        parameters = either method a mbid

getTopFans :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopFans a mbid autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "track.getTopFans"
        parameters = either method a mbid

getTopTags :: Maybe (Artist, Track) -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a mbid autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "track.getTopTags"
        parameters = either method a mbid

love :: Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
love track artist apiKey sessionKey = dispatch $ callAPI_ "track.love"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

removeTag :: Artist -> Track -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist track tag apiKey sessionKey = dispatch $ callAPI_ "track.removeTag"
  [ "artist" ?< artist
  , "track" ?< track
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

scrobble :: ( Timestamp, Maybe Album, Track, Artist, Maybe AlbumArtist
           , Maybe Duration, Maybe StreamId, Maybe ChosenByUser
           , Maybe Context, Maybe TrackNumber, Maybe Mbid )
         -> APIKey
         -> SessionKey
         -> Lastfm ()
scrobble (timestamp, album, track, artist, albumArtist, duration, streamId, chosenByUser, context, trackNumber, mbid) apiKey sessionKey = dispatch $ callAPI_ "track.scrobble"
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

search :: Track -> Maybe Page -> Maybe Limit -> Maybe Artist -> APIKey -> Lastfm Response
search limit page track artist apiKey = dispatch $ callAPI "track.search"
  [ "track" ?< track
  , "page" ?< page
  , "limit" ?< limit
  , "artist" ?< artist
  , "api_key" ?< apiKey
  ]

share :: Artist -> Track -> Maybe Public -> Maybe Message -> [Recipient] -> APIKey -> SessionKey -> Lastfm ()
share artist track public message recipients apiKey sessionKey = dispatch go
  where go
          | null recipients        = throw $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = callAPI_ method
            [ "artist" ?< artist
            , "track" ?< track
            , "recipient" ?< recipients
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            , "public" ?< public
            , "message" ?< message
            ]
            where method = "track.share"

unban :: Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
unban track artist apiKey sessionKey = dispatch $ callAPI_ "track.unban"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

unlove :: Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
unlove track artist apiKey sessionKey = dispatch $ callAPI_ "track.unlove"
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
updateNowPlaying track artist album albumArtist context trackNumber mbid duration apiKey sessionKey = dispatch $ callAPI_ "track.updateNowPlaying"
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

either :: String -> Maybe (Artist, Track) -> Maybe Mbid -> [(String, String)]
either method a mbid
  | isJust mbid = [ "mbid" ?< mbid ]
  | otherwise   = case a of
                    Just (artist, track) -> [ "artist" ?< artist, "track" ?< track ]
                    Nothing              -> throw $ WrapperCallError method "no mbid nor (artist, track) are specified."
