module Network.Lastfm.Track
  ( love, unlove
  , updateNowPlaying
  ) where

import Data.Maybe (fromMaybe)

import Network.Lastfm.Core

type APIKey = String
type Artist = String
type Track = String
type SessionKey = String
type Album = String
type AlbumArtist = String
type Context = String
type TrackNumber = String
type Mbid = String
type Duration = String

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

optional :: String -> Maybe a -> [(String, a)]
optional key value = case value of
                       Just v  -> [(key, v)]
                       Nothing -> []
