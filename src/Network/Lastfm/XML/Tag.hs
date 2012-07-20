{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Tag API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

#include "tag.docs"

import Network.Lastfm.Internal
import Network.Lastfm.XML (xmlWrapper)
import qualified Network.Lastfm.API.Tag as API

$(xmlWrapper ["getInfo", "getSimilar", "getTopAlbums", "getTopArtists", "getTopTags", "getTopTracks", "getWeeklyArtistChart", "getWeeklyChartList", "search"])

__getInfo__
getInfo ∷ Tag → Maybe Language → APIKey → Lastfm Response

__getSimilar__
getSimilar ∷ Tag → APIKey → Lastfm Response

__getTopAlbums__
getTopAlbums ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopArtists__
getTopArtists ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopTags__
getTopTags ∷ APIKey → Lastfm Response

__getTopTracks__
getTopTracks ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getWeeklyArtistChart__
getWeeklyArtistChart ∷ Tag → Maybe From → Maybe To → Maybe Limit → APIKey → Lastfm Response

__getWeeklyChartList__
getWeeklyChartList ∷ Tag → APIKey → Lastfm Response

__search__
search ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
