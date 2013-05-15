{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Network.Lastfm.Request


-- | Add an album or collection of albums to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addAlbum>
addAlbum :: Request f Sign (Artist -> Album -> APIKey -> SessionKey -> Ready)
addAlbum = api "library.addAlbum" <* post
{-# INLINE addAlbum #-}


-- | Add an artist to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addArtist>
addArtist :: Request f Sign (Artist -> APIKey -> SessionKey -> Ready)
addArtist = api "library.addArtist" <* post
{-# INLINE addArtist #-}


-- | Add a track to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addTrack>
addTrack :: Request f Sign (Artist -> Track -> APIKey -> SessionKey -> Ready)
addTrack = api "library.addTrack" <* post
{-# INLINE addTrack #-}


-- | A paginated list of all the albums in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getAlbums>
getAlbums :: Request f Send (User -> APIKey -> Ready)
getAlbums = api "library.getAlbums"
{-# INLINE getAlbums #-}


-- | A paginated list of all the artists in a user's library, with play counts and tag counts.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getArtists>
getArtists :: Request f Send (User -> APIKey -> Ready)
getArtists = api "library.getArtists"
{-# INLINE getArtists #-}


-- | A paginated list of all the tracks in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'album', 'page', 'limit'
--
-- <http://www.last.fm/api/show/library.getTracks>
getTracks :: Request f Send (User -> APIKey -> Ready)
getTracks = api "library.getTracks"
{-# INLINE getTracks #-}


-- | Remove an album from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeAlbum>
removeAlbum :: Request f Sign (Artist -> Album -> APIKey -> SessionKey -> Ready)
removeAlbum = api "library.removeAlbum" <* post
{-# INLINE removeAlbum #-}


-- | Remove an artist from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeArtist>
removeArtist :: Request f Sign (Artist -> APIKey -> SessionKey -> Ready)
removeArtist = api "library.removeArtist" <* post
{-# INLINE removeArtist #-}


-- | Remove a scrobble from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeScrobble>
removeScrobble :: Request f Sign (Artist -> Track -> Timestamp -> APIKey -> SessionKey -> Ready)
removeScrobble = api "library.removeScrobble" <* post
{-# INLINE removeScrobble #-}


-- | Remove a track from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeTrack>
removeTrack :: Request f Sign (Artist -> Track -> APIKey -> SessionKey -> Ready)
removeTrack = api "library.removeTrack" <* post
{-# INLINE removeTrack #-}
