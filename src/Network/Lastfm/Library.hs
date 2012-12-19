{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm library API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Library as Library
-- @
module Network.Lastfm.Library
  ( addAlbum, addArtist, addTrack
  , getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

import Control.Applicative

import Data.Void (Void)

import Network.Lastfm.Request


-- | Add an album or collection of albums to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addAlbum>
addAlbum ∷ Request f RequireSign (Artist → Album → APIKey → SessionKey → Void)
addAlbum = api "library.addAlbum" <* post


-- | Add an artist to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addArtist>
addArtist ∷ Request f RequireSign (Artist → APIKey → SessionKey → Void)
addArtist = api "library.addArtist" <* post


-- | Add a track to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addTrack>
addTrack ∷ Request f RequireSign (Artist → Track → APIKey → SessionKey → Void)
addTrack = api "library.addTrack" <* post


-- | A paginated list of all the albums in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getAlbums>
getAlbums ∷ Request f Ready (User → APIKey → Void)
getAlbums = api "library.getAlbums"


-- | A paginated list of all the artists in a user's library, with play counts and tag counts.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getArtists>
getArtists ∷ Request f Ready (User → APIKey → Void)
getArtists = api "library.getArtists"


-- | A paginated list of all the tracks in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'album', 'page', 'limit'
--
-- <http://www.last.fm/api/show/library.getTracks>
getTracks ∷ Request f Ready (User → APIKey → Void)
getTracks = api "library.getTracks"


-- | Remove an album from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeAlbum>
removeAlbum ∷ Request f RequireSign (Artist → Album → APIKey → SessionKey → Void)
removeAlbum = api "library.removeAlbum" <* post


-- | Remove an artist from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeArtist>
removeArtist ∷ Request f RequireSign (Artist → APIKey → SessionKey → Void)
removeArtist = api "library.removeArtist" <* post


-- | Remove a scrobble from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeScrobble>
removeScrobble ∷ Request f RequireSign (Artist → Track → Timestamp → APIKey → SessionKey → Void)
removeScrobble = api "library.removeScrobble" <* post


-- | Remove a track from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeTrack>
removeTrack ∷ Request f RequireSign (Artist → Track → APIKey → SessionKey → Void)
removeTrack = api "library.removeTrack" <* post
