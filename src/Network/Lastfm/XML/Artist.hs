{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Artist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Artist
  ( addTags, getCorrection, getEvents, getImages, getInfo
  , getPastEvents, getPodcast, getShouts, getSimilar, getTags, getTopAlbums
  , getTopFans, getTopTags, getTopTracks, removeTag, search, share, shout
  ) where

#include "artist.docs"

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Artist as API

$(xml ["addTags", "getCorrection", "getEvents", "getImages", "getInfo", "getPastEvents", "getPodcast", "getShouts", "getSimilar", "getTags", "getTopAlbums", "getTopFans", "getTopTags", "getTopTracks", "removeTag", "search", "share", "shout"])

__addTags__
addTags ∷ Artist → [Tag] → APIKey → SessionKey → Secret → Lastfm Response

__getCorrection__
getCorrection ∷ Artist → APIKey → Lastfm Response

__getEvents__
getEvents ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → Maybe FestivalsOnly → APIKey → Lastfm Response

__getImages__
getImages ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → Maybe Order → APIKey → Lastfm Response

__getInfo__
getInfo ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Language → Maybe Username → APIKey → Lastfm Response

__getPastEvents__
getPastEvents ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getPodcast__
getPodcast ∷ Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response

__getShouts__
getShouts ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getSimilar__
getSimilar ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Limit → APIKey → Lastfm Response

__getTags__
getTags ∷ Either Artist Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response

__getTopAlbums__
getTopAlbums ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopFans__
getTopFans ∷ Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response

__getTopTags__
getTopTags ∷ Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response

__getTopTracks__
getTopTracks ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__removeTag__
removeTag ∷ Artist → Tag → APIKey → SessionKey → Secret → Lastfm Response

__search__
search ∷ Artist → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__share__
share ∷ Artist → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response

__shout__
shout ∷ Artist → Message → APIKey → SessionKey → Secret → Lastfm Response
