-- | Playlist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Playlist
  ( addTrack, create
  ) where

import Network.Lastfm

-- | Add a track to a Last.fm user's playlist.
--
-- More: <http://www.last.fm/api/show/playlist.addTrack>
addTrack ∷ Playlist → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
addTrack playlist artist track apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "playlist.addTrack")
  , (#) playlist
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Create a Last.fm playlist on behalf of a user.
--
-- More: <http://www.last.fm/api/show/playlist.create>
create ∷ Maybe Title → Maybe Description → APIKey → SessionKey → Secret → Lastfm Response
create title description apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "playlist.create")
  , (#) title
  , (#) description
  , (#) apiKey
  , (#) sessionKey
  ]
