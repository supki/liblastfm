-- | Library API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Library
  ( addAlbum, addArtist, addTrack, getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

import Control.Monad.Error (runErrorT)
import Network.Lastfm

-- | Add an album or collection of albums to a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.addAlbum>
addAlbum :: Artist -> Album -> APIKey -> SessionKey -> Secret -> Lastfm Response
addAlbum artist album apiKey sessionKey secret = callAPIsigned secret
  [ (#) (Method "library.addAlbum")
  , (#) artist
  , (#) album
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Add an artist to a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.addArtist>
addArtist :: Artist -> APIKey -> SessionKey -> Secret -> Lastfm Response
addArtist artist apiKey sessionKey secret = callAPIsigned secret
  [ (#) (Method "library.addArtist")
  , (#) artist
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Add a track to a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.addTrack>
addTrack :: Artist -> Track -> APIKey -> SessionKey -> Secret -> Lastfm Response
addTrack artist track apiKey sessionKey secret = callAPIsigned secret
  [ (#) (Method "library.addTrack")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

-- | A paginated list of all the albums in a user's library, with play counts and tag counts.
--
-- More: <http://www.lastfm.ru/api/show/library.getAlbums>
getAlbums :: User -> Maybe Artist -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getAlbums user artist page limit apiKey = callAPI
  [ (#) (Method "library.getAlbums")
  , (#) user
  , (#) artist
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | A paginated list of all the artists in a user's library, with play counts and tag counts.
--
-- More: <http://www.lastfm.ru/api/show/library.getArtists>
getArtists :: User -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getArtists user page limit apiKey = callAPI
  [ (#) (Method "library.getArtists")
  , (#) user
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | A paginated list of all the tracks in a user's library, with play counts and tag counts.
--
-- More: <http://www.lastfm.ru/api/show/library.getTracks>
getTracks :: User -> Maybe Artist -> Maybe Album -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTracks user artist album page limit apiKey = callAPI
  [ (#) (Method "library.getTracks")
  , (#) user
  , (#) artist
  , (#) album
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Remove an album from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeAlbum>
removeAlbum :: Artist -> Album -> APIKey -> SessionKey -> Secret -> Lastfm Response
removeAlbum artist album apiKey sessionKey secret = callAPIsigned secret
  [ (#) (Method "library.removeAlbum")
  , (#) artist
  , (#) album
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Remove an artist from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeArtist>
removeArtist :: Artist -> APIKey -> SessionKey -> Secret -> Lastfm Response
removeArtist artist apiKey sessionKey secret = callAPIsigned secret
  [ (#) (Method "library.removeArtist")
  , (#) artist
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Remove a scrobble from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeScrobble>
removeScrobble :: Artist -> Track -> Timestamp -> APIKey -> SessionKey -> Secret -> Lastfm Response
removeScrobble artist track timestamp apiKey sessionKey secret = callAPIsigned secret
  [ (#) (Method "library.removeScrobble")
  , (#) artist
  , (#) track
  , (#) timestamp
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Remove a track from a user's Last.fm library.
--
-- More: <http://www.lastfm.ru/api/show/library.removeTrack>
removeTrack :: Artist -> Track -> APIKey -> SessionKey -> Secret -> Lastfm Response
removeTrack artist track apiKey sessionKey secret = callAPIsigned secret
  [ (#) (Method "library.removeTrack")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]
