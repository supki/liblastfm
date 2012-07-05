{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Library API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Library
  ( addAlbum, addArtist, addTrack, getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

#include "library.docs"

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Library as API

$(xml ["addAlbum", "addArtist", "addTrack", "getAlbums", "getArtists", "getTracks", "removeAlbum", "removeArtist", "removeScrobble", "removeTrack"])

__addAlbum__
addAlbum ∷ Artist → Album → APIKey → SessionKey → Secret → Lastfm Response

__addArtist__
addArtist ∷ Artist → APIKey → SessionKey → Secret → Lastfm Response

__addTrack__
addTrack ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

__getAlbums__
getAlbums ∷ User → Maybe Artist → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getArtists__
getArtists ∷ User → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTracks__
getTracks ∷ User → Maybe Artist → Maybe Album → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__removeAlbum__
removeAlbum ∷ Artist → Album → APIKey → SessionKey → Secret → Lastfm Response

__removeArtist__
removeArtist ∷ Artist → APIKey → SessionKey → Secret → Lastfm Response

__removeScrobble__
removeScrobble ∷ Artist → Track → Timestamp → APIKey → SessionKey → Secret → Lastfm Response

__removeTrack__
removeTrack ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
