{-# LANGUAGE TemplateHaskell #-}
-- | Library API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Library
  ( addAlbum, addArtist, addTrack, getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

import Network.Lastfm
import qualified Network.Lastfm.API.Library as API
  
$(json ["addAlbum", "addArtist", "addTrack", "getAlbums", "getArtists", "getTracks", "removeAlbum", "removeArtist", "removeScrobble", "removeTrack"])

-- | Add an album or collection of albums to a user's Last.fm library.
--
-- More: <http://www.last.fm/api/show/library.addAlbum>
addAlbum ∷ Artist → Album → APIKey → SessionKey → Secret → Lastfm Response

-- | Add an artist to a user's Last.fm library.
--
-- More: <http://www.last.fm/api/show/library.addArtist>
addArtist ∷ Artist → APIKey → SessionKey → Secret → Lastfm Response

-- | Add a track to a user's Last.fm library.
--
-- More: <http://www.last.fm/api/show/library.addTrack>
addTrack ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

-- | A paginated list of all the albums in a user's library, with play counts and tag counts.
--
-- More: <http://www.last.fm/api/show/library.getAlbums>
getAlbums ∷ User → Maybe Artist → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | A paginated list of all the artists in a user's library, with play counts and tag counts.
--
-- More: <http://www.last.fm/api/show/library.getArtists>
getArtists ∷ User → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | A paginated list of all the tracks in a user's library, with play counts and tag counts.
--
-- More: <http://www.last.fm/api/show/library.getTracks>
getTracks ∷ User → Maybe Artist → Maybe Album → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Remove an album from a user's Last.fm library.
--
-- More: <http://www.last.fm/api/show/library.removeAlbum>
removeAlbum ∷ Artist → Album → APIKey → SessionKey → Secret → Lastfm Response

-- | Remove an artist from a user's Last.fm library.
--
-- More: <http://www.last.fm/api/show/library.removeArtist>
removeArtist ∷ Artist → APIKey → SessionKey → Secret → Lastfm Response

-- | Remove a scrobble from a user's Last.fm library.
--
-- More: <http://www.last.fm/api/show/library.removeScrobble>
removeScrobble ∷ Artist → Track → Timestamp → APIKey → SessionKey → Secret → Lastfm Response

-- | Remove a track from a user's Last.fm library.
--
-- More: <http://www.last.fm/api/show/library.removeTrack>
removeTrack ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
