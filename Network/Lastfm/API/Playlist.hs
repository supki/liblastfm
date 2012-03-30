-- | Playlist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Playlist
  ( addTrack, create
  ) where

import Control.Monad (void)

import Network.Lastfm

-- | Add a track to a Last.fm user's playlist.
--
-- More: <http://www.lastfm.ru/api/show/playlist.addTrack>
addTrack :: Playlist -> Artist -> Track -> APIKey -> SessionKey -> Secret -> Lastfm ()
addTrack playlist artist track apiKey sessionKey secret = dispatch . void . callAPIsigned secret $
  [ "method" ?< "playlist.addTrack"
  , "playlistID" ?< playlist
  , "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Create a Last.fm playlist on behalf of a user.
--
-- More: <http://www.lastfm.ru/api/show/playlist.create>
create :: Maybe Title -> Maybe Description -> APIKey -> SessionKey -> Secret -> Lastfm ()
create title description apiKey sessionKey secret = dispatch . void . callAPIsigned secret $
  [ "method" ?< "playlist.create"
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  , "title" ?< title
  , "description" ?< description
  ]
