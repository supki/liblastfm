{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lastfm playlist API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Playlist as Playlist
-- @
module Network.Lastfm.Playlist
  ( addTrack, create
  ) where

import Control.Applicative

import Network.Lastfm.Request


-- | Add a track to a Last.fm user's playlist
--
-- <http://www.last.fm/api/show/playlist.addTrack>
addTrack :: Request f (Playlist -> Artist -> Track -> APIKey -> SessionKey -> Sign)
addTrack = api "playlist.addTrack" <* post
{-# INLINE addTrack #-}


-- | Create a Last.fm playlist on behalf of a user
--
-- Optional: 'title', 'description'
--
-- <http://www.last.fm/api/show/playlist.create>
create :: Request f (APIKey -> SessionKey -> Sign)
create = api "playlist.create" <* post
{-# INLINE create #-}
