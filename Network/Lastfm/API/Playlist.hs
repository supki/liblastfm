-- | Playlist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Playlist
  ( addTrack, create
  ) where

import Control.Monad (void)

import Network.Lastfm ( Lastfm, callAPI, dispatch
                      , (?<), APIKey, Artist, Playlist, SessionKey, Title, Description, Track
                      )

-- | Add a track to a Last.fm user's playlist.
--
-- More: <http://www.lastfm.ru/api/show/playlist.addTrack>
addTrack :: Playlist -> Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
addTrack playlist artist track apiKey sessionKey = dispatch $ void $ callAPI "playlist.addTrack"
  [ "playlistID" ?< playlist
  , "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Create a Last.fm playlist on behalf of a user.
--
-- More: <http://www.lastfm.ru/api/show/playlist.create>
create :: Maybe Title -> Maybe Description -> APIKey -> SessionKey -> Lastfm ()
create title description apiKey sessionKey = dispatch $ void $ callAPI "playlist.create"
  [ "api_key" ?< apiKey
  , "sk" ?< sessionKey
  , "title" ?< title
  , "description" ?< description
  ]
