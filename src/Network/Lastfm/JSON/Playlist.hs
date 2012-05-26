{-# LANGUAGE TemplateHaskell #-}
-- | Playlist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Playlist
  ( addTrack, create
  ) where

import Network.Lastfm
import qualified Network.Lastfm.API.Playlist as API

$(json ["addTrack", "create"])

-- | Add a track to a Last.fm user's playlist.
--
-- More: <http://www.last.fm/api/show/playlist.addTrack>
addTrack ∷ Playlist → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

-- | Create a Last.fm playlist on behalf of a user.
--
-- More: <http://www.last.fm/api/show/playlist.create>
create ∷ Maybe Title → Maybe Description → APIKey → SessionKey → Secret → Lastfm Response
