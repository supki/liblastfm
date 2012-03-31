-- | Playlist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Playlist
  ( addTrack, create
  ) where

import Control.Monad (void)
import Control.Monad.Error (runErrorT)
import Network.Lastfm

-- | Add a track to a Last.fm user's playlist.
--
-- More: <http://www.lastfm.ru/api/show/playlist.addTrack>
addTrack :: Playlist -> Artist -> Track -> APIKey -> SessionKey -> Secret -> Lastfm ()
addTrack playlist artist track apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ (#) (Method "playlist.addTrack")
  , (#) playlist
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Create a Last.fm playlist on behalf of a user.
--
-- More: <http://www.lastfm.ru/api/show/playlist.create>
create :: Maybe Title -> Maybe Description -> APIKey -> SessionKey -> Secret -> Lastfm ()
create title description apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ (#) (Method "playlist.create")
  , (#) title
  , (#) description
  , (#) apiKey
  , (#) sessionKey
  ]
