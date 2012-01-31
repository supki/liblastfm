module Network.Lastfm.Track
  ( ban, unban
  , love, unlove
  , scrobble, updateNowPlaying
  ) where

import Network.Lastfm.Core

type APIKey = String
type Artist = String
type Track = String
type SessionKey = String
type Album = String
type AlbumArtist = String
type Context = String
type TrackNumber = String
type Timestamp = String
type StreamId = String
type Mbid = String
type Duration = String
type ChosenByUser = String

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

optional :: String -> Maybe a -> [(String, a)]
optional key value = case value of
                       Just v  -> [(key, v)]
                       Nothing -> []
