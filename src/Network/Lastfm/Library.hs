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
  ( addAlbum, addArtist, addTrack, getAlbums, getArtists, getTracks, removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Add an album or collection of albums to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addAlbum>
addAlbum ∷ Artist → Album → Request f RequireSign
addAlbum ar al = api "library.addAlbum" <> artist ar <> album al <> post


-- | Add an artist to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addArtist>
addArtist ∷ Artist → Request f RequireSign
addArtist a = api "library.addArtist" <> artist a <> post


-- | Add a track to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addTrack>
addTrack ∷ Artist → Track → Request f RequireSign
addTrack a t = api "library.addTrack" <> artist a <> track t <> post


-- | A paginated list of all the albums in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getAlbums>
getAlbums ∷ User → Request f Ready
getAlbums u = api "library.getAlbums" <> user u


-- | A paginated list of all the artists in a user's library, with play counts and tag counts.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getArtists>
getArtists ∷ User → Request f Ready
getArtists u = api "library.getArtists" <> user u


-- | A paginated list of all the tracks in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'album', 'page', 'limit'
--
-- <http://www.last.fm/api/show/library.getTracks>
getTracks ∷ User → Request f Ready
getTracks u = api "library.getTracks" <> user u


-- | Remove an album from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeAlbum>
removeAlbum ∷ Artist → Album → Request f RequireSign
removeAlbum ar al = api "library.removeAlbum" <> artist ar <> album al <> post


-- | Remove an artist from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeArtist>
removeArtist ∷ Artist → Request f RequireSign
removeArtist a = api "library.removeArtist" <> artist a <> post


-- | Remove a scrobble from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeScrobble>
removeScrobble ∷ Artist → Track → Timestamp → Request f RequireSign
removeScrobble a tr ts = api "library.removeScrobble" <> artist a <> track tr <> timestamp ts <> post


-- | Remove a track from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeTrack>
removeTrack ∷ Artist → Track → Request f RequireSign
removeTrack a t = api "library.removeTrack" <> artist a <> track t <> post
