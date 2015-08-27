{-# LANGUAGE CPP #-}
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
  ( addAlbum, albumItem, addArtist, artistItem, addTrack
  , getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N

import           Network.Lastfm.Internal (absorbQuery, indexedWith, wrap)
import           Network.Lastfm.Request


-- | Add an album or collection of albums to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addAlbum>
addAlbum :: NonEmpty (Request f LibraryAlbum) -> Request f (APIKey -> SessionKey -> Sign)
addAlbum batch = api "library.addAlbum" <* items <* post
 where
  items = absorbQuery (N.zipWith indexedWith (N.fromList [0..]) batch)

-- | What artist to add to library?
albumItem :: Request f (Artist -> Album -> LibraryAlbum)
albumItem = wrap id


-- | Add an artist to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addArtist>
addArtist :: NonEmpty (Request f LibraryArtist) -> Request f (APIKey -> SessionKey -> Sign)
addArtist batch = api "library.addArtist" <* items <* post
 where
  items = absorbQuery (N.zipWith indexedWith (N.fromList [0..]) batch)

-- | What album to add to library?
artistItem :: Request f (Artist -> LibraryArtist)
artistItem = wrap id


-- | Add a track to a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.addTrack>
addTrack :: Request f (Artist -> Track -> APIKey -> SessionKey -> Sign)
addTrack = api "library.addTrack" <* post


-- | A paginated list of all the albums in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getAlbums>
getAlbums :: Request f (User -> APIKey -> Ready)
getAlbums = api "library.getAlbums"


-- | A paginated list of all the artists in a user's library, with play counts and tag counts.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/library.getArtists>
getArtists :: Request f (User -> APIKey -> Ready)
getArtists = api "library.getArtists"


-- | A paginated list of all the tracks in a user's library, with play counts and tag counts.
--
-- Optional: 'artist', 'album', 'page', 'limit'
--
-- <http://www.last.fm/api/show/library.getTracks>
getTracks :: Request f (User -> APIKey -> Ready)
getTracks = api "library.getTracks"


-- | Remove an album from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeAlbum>
removeAlbum :: Request f (Artist -> Album -> APIKey -> SessionKey -> Sign)
removeAlbum = api "library.removeAlbum" <* post


-- | Remove an artist from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeArtist>
removeArtist :: Request f (Artist -> APIKey -> SessionKey -> Sign)
removeArtist = api "library.removeArtist" <* post


-- | Remove a scrobble from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeScrobble>
removeScrobble :: Request f (Artist -> Track -> Timestamp -> APIKey -> SessionKey -> Sign)
removeScrobble = api "library.removeScrobble" <* post


-- | Remove a track from a user's Last.fm library
--
-- <http://www.last.fm/api/show/library.removeTrack>
removeTrack :: Request f (Artist -> Track -> APIKey -> SessionKey -> Sign)
removeTrack = api "library.removeTrack" <* post
