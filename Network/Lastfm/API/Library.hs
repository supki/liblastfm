-- | Library API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Library
  ( addAlbum, addArtist, addTrack, getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

import Control.Monad (void)
import Control.Monad.Error (runErrorT)
import Network.Lastfm

-- | Add an album or collection of albums to a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.addAlbum>
addAlbum :: Artist -> Album -> APIKey -> SessionKey -> Secret -> Lastfm ()
addAlbum artist album apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ "method" ?< "library.addAlbum"
  , "artist" ?< artist
  , "album" ?< album
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Add an artist to a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.addArtist>
addArtist :: Artist -> APIKey -> SessionKey -> Secret -> Lastfm ()
addArtist artist apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ "method" ?< "library.addArtist"
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Add a track to a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.addTrack>
addTrack :: Artist -> Track -> APIKey -> SessionKey -> Secret -> Lastfm ()
addTrack artist track apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ "method" ?< "library.addTrack"
  , "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | A paginated list of all the albums in a user's library, with play counts and tag counts.
--
-- More: <http://www.lastfm.ru/api/show/library.getAlbums>
getAlbums :: User -> Maybe Artist -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getAlbums user artist page limit apiKey = runErrorT . callAPI $
  [ "method" ?< "library.getAlbums"
  , "user" ?< user
  , "artist" ?< artist
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | A paginated list of all the artists in a user's library, with play counts and tag counts.
--
-- More: <http://www.lastfm.ru/api/show/library.getArtists>
getArtists :: User -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getArtists user page limit apiKey = runErrorT . callAPI $
  [ "method" ?< "library.getArtists"
  , "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | A paginated list of all the tracks in a user's library, with play counts and tag counts.
--
-- More: <http://www.lastfm.ru/api/show/library.getTracks>
getTracks :: User -> Maybe Artist -> Maybe Album -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTracks user artist album page limit apiKey = runErrorT . callAPI $
  [ "method" ?< "library.getTracks"
  , "user" ?< user
  , "artist" ?< artist
  , "album" ?< album
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Remove an album from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeAlbum>
removeAlbum :: Artist -> Album -> APIKey -> SessionKey -> Secret -> Lastfm ()
removeAlbum artist album apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ "method" ?< "library.removeAlbum"
  , "artist" ?< artist
  , "album" ?< album
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Remove an artist from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeArtist>
removeArtist :: Artist -> APIKey -> SessionKey -> Secret -> Lastfm ()
removeArtist artist apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ "method" ?< "library.removeArtist"
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Remove a scrobble from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeScrobble>
removeScrobble :: Artist -> Track -> Timestamp -> APIKey -> SessionKey -> Secret -> Lastfm ()
removeScrobble artist track timestamp apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ "method" ?< "library.removeScrobble"
  , "artist" ?< artist
  , "track" ?< track
  , "timestamp" ?< timestamp
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Remove a track from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeTrack>
removeTrack :: Artist -> Track -> APIKey -> SessionKey -> Secret -> Lastfm ()
removeTrack artist track apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ "method" ?< "library.removeTrack"
  , "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
