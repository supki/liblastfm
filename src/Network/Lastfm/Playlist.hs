{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Playlist
  ( addTrack, create
  ) where

import Control.Applicative

import Network.Lastfm.Request


-- | Add a track to a Last.fm user's playlist
--
-- <http://www.last.fm/api/show/playlist.addTrack>
addTrack ∷ Request f Sign (Playlist → Artist → Track → APIKey → SessionKey → Ready)
addTrack = api "playlist.addTrack" <* post


-- | Create a Last.fm playlist on behalf of a user
--
-- Optional: 'title', 'description'
--
-- <http://www.last.fm/api/show/playlist.create>
create ∷ Request f Sign (APIKey → SessionKey → Ready)
create = api "playlist.create" <* post
