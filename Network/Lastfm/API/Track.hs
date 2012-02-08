module Network.Lastfm.API.Track
  ( addTags, ban, getBuyLinks, getCorrection, getFingerprintMetadata
  , getInfo, getShouts, getSimilar, getTags, getTopFans, getTopTags
  , love, removeTag, scrobble, search, share, unban, unlove, updateNowPlaying
  ) where

import Control.Exception (throw)

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


getBuyLinks :: Either (Artist, Track) Mbid -> Maybe Autocorrect -> Maybe Country -> APIKey -> Lastfm Response
getBuyLinks a autocorrect country apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "country" ?< country
  , "api_key" ?< apiKey
  ]
  where method = "track.getBuyLinks"
        target = case a of
                   Left (artist, track) -> ["artist" ?< artist, "track" ?< track]
                   Right mbid           -> ["mbid" ?< mbid]

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

getInfo :: Either (Artist, Track) Mbid -> Maybe Autocorrect -> Maybe User -> APIKey -> Lastfm Response
getInfo a autocorrect username apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "username" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "track.getInfo"
        target = case a of
                   Left (artist, track) -> ["artist" ?< artist, "track" ?< track]
                   Right mbid           -> ["mbid" ?< mbid]

getShouts :: Either (Artist, Track) Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts a autocorrect page limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "track.getShouts"
        target = case a of
                   Left (artist, track) -> ["artist" ?< artist, "track" ?< track]
                   Right mbid           -> ["mbid" ?< mbid]

getSimilar :: Either (Artist, Track) Mbid -> Maybe Autocorrect -> Maybe Limit -> APIKey -> Lastfm Response
getSimilar a autocorrect limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "track.getSimilar"
        target = case a of
                   Left (artist, track) -> ["artist" ?< artist, "track" ?< track]
                   Right mbid           -> ["mbid" ?< mbid]

getTags :: Either (Artist, Track) Mbid -> Maybe Autocorrect -> Maybe User -> APIKey -> Lastfm Response
getTags a autocorrect username apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "user" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "track.getTags"
        target = case a of
                   Left (artist, track) -> ["artist" ?< artist, "track" ?< track]
                   Right mbid           -> ["mbid" ?< mbid]

getTopFans :: Either (Artist, Track) Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopFans a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "track.getTopFans"
        target = case a of
                   Left (artist, track) -> ["artist" ?< artist, "track" ?< track]
                   Right mbid           -> ["mbid" ?< mbid]

getTopTags :: Either (Artist, Track) Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "track.getTopTags"
        target = case a of
                   Left (artist, track) -> ["artist" ?< artist, "track" ?< track]
                   Right mbid           -> ["mbid" ?< mbid]

love :: Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
love artist track apiKey sessionKey = dispatch $ callAPI_ "track.love"
  [ "artist" ?< artist
  , "track" ?< track
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

scrobble :: ( Timestamp, Maybe Album, Artist, Track, Maybe AlbumArtist
           , Maybe Duration, Maybe StreamId, Maybe ChosenByUser
           , Maybe Context, Maybe TrackNumber, Maybe Mbid )
         -> APIKey
         -> SessionKey
         -> Lastfm ()
scrobble (timestamp, album, artist, track, albumArtist, duration, streamId, chosenByUser, context, trackNumber, mbid) apiKey sessionKey = dispatch $ callAPI_ "track.scrobble"
  [ "timestamp" ?< timestamp
  , "artist" ?< artist
  , "track" ?< track
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

share :: Artist -> Track -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Lastfm ()
share artist track recipients message public apiKey sessionKey = dispatch go
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

unban :: Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
unban artist track apiKey sessionKey = dispatch $ callAPI_ "track.unban"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

unlove :: Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
unlove artist track apiKey sessionKey = dispatch $ callAPI_ "track.unlove"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

updateNowPlaying :: Artist
                 -> Track
                 -> Maybe Album
                 -> Maybe AlbumArtist
                 -> Maybe Context
                 -> Maybe TrackNumber
                 -> Maybe Mbid
                 -> Maybe Duration
                 -> APIKey
                 -> SessionKey
                 -> Lastfm ()
updateNowPlaying artist track album albumArtist context trackNumber mbid duration apiKey sessionKey = dispatch $ callAPI_ "track.updateNowPlaying"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  , "album" ?< album
  , "albumArtist" ?< albumArtist
  , "context" ?< context
  , "trackNumber" ?< trackNumber
  , "mbid" ?< mbid
  , "duration" ?< duration
  ]
