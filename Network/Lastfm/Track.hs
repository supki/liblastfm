module Network.Lastfm.Track
  ( ban, unban
  , love, unlove
  , scrobble, updateNowPlaying
  ) where

import Data.List (intercalate)

import Network.Lastfm.Core

type APIKey = String
type SessionKey = String

type Album = String
type AlbumArtist = String
type Artist = String
type ChosenByUser = String
type Context = String
type Duration = String
type Limit = String
type Mbid = String
type Message = String
type Page = String
type Public = String
type Recipient = String
type StreamId = String
type Tag = String
type Timestamp = String
type Track = String
type TrackNumber = String

ban :: Track -> Artist -> APIKey -> SessionKey -> IO ()
ban track artist apiKey sessionKey = callAPI_
  [ ("method","track.ban")
  , ("track", track)
  , ("artist", artist)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ]

unban :: Track -> Artist -> APIKey -> SessionKey -> IO ()
unban track artist apiKey sessionKey = callAPI_
  [ ("method","track.unban")
  , ("track", track)
  , ("artist", artist)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ]

love :: Track -> Artist -> APIKey -> SessionKey -> IO ()
love track artist apiKey sessionKey = callAPI_
  [ ("method","track.love")
  , ("track", track)
  , ("artist", artist)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ]

unlove :: Track -> Artist -> APIKey -> SessionKey -> IO ()
unlove track artist apiKey sessionKey = callAPI_
  [ ("method","track.unlove")
  , ("track", track)
  , ("artist", artist)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
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
                 -> IO ()
updateNowPlaying track artist album albumArtist context trackNumber mbid duration apiKey sessionKey = callAPI_ $
  [ ("method","track.updateNowPlaying")
  , ("track", track)
  , ("artist", artist)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ] ++
  optional "album" album ++
  optional "albumArtist" albumArtist ++
  optional "context" context ++
  optional "trackNumber" trackNumber ++
  optional "mbid" mbid ++
  optional "duration" duration

scrobble :: [ ( Timestamp, Maybe Album, Track, Artist, Maybe AlbumArtist
           , Maybe Duration, Maybe StreamId, Maybe ChosenByUser
           , Maybe Context, Maybe TrackNumber, Maybe Mbid ) ]
         -> APIKey
         -> SessionKey
         -> IO ()
scrobble xs apiKey sessionKey = mapM_ scrobbleTrack xs
  where scrobbleTrack (timestamp, album, track, artist, albumArtist, duration, streamId, chosenByUser, context, trackNumber, mbid) = callAPI_ $
          [ ("method","track.scrobble")
          , ("timestamp", timestamp)
          , ("track", track)
          , ("artist", artist)
          , ("api_key", apiKey)
          , ("sk", sessionKey)
          ] ++
          optional "album" album ++
          optional "albumArtist" albumArtist ++
          optional "duration" duration ++
          optional "streamId" streamId ++
          optional "chosenByUser" chosenByUser ++
          optional "context" context ++
          optional "trackNumber" trackNumber ++
          optional "mbid" mbid

search :: Maybe Limit -> Maybe Page -> Track -> Maybe Artist -> APIKey -> IO Response
search limit page track artist apiKey = callAPI $
  [ ("method","track.search")
  , ("track", track)
  , ("api_key", apiKey)
  ] ++
  optional "limit" limit ++
  optional "page" page ++
  optional "artist" artist

share :: Artist -> Track -> Maybe Public -> Maybe Message -> [Recipient] -> APIKey -> SessionKey -> IO ()
share artist track public message recipients apiKey sessionKey = callAPI_ $
  [ ("method","track.share")
  , ("artist", artist)
  , ("track", track)
  , ("recipient", intercalate "," recipients)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ] ++
  optional "public" public ++
  optional "message" message

addTags :: Artist -> Track -> [Tag] -> APIKey -> SessionKey -> IO ()
addTags artist track tags apiKey sessionKey
  | null tags        = error "Track.addTags: empty tag list."
  | length tags > 10 = error "Track.addTags: tag list length has exceeded maximum."
  | otherwise        = callAPI_ $
    [ ("method","track.addTags")
    , ("artist", artist)
    , ("track", track)
    , ("tags", intercalate "," tags)
    , ("api_key", apiKey)
    , ("sk", sessionKey)
    ]

removeTag :: Artist -> Track -> Tag -> APIKey -> SessionKey -> IO ()
removeTag artist track tag apiKey sessionKey = callAPI_ $
  [ ("method","track.removeTag")
  , ("artist", artist)
  , ("track", track)
  , ("tag", tag)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ]

optional :: String -> Maybe a -> [(String, a)]
optional key value = case value of
                       Just v  -> [(key, v)]
                       Nothing -> []
