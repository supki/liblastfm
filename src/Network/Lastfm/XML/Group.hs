{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Group API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Group
  ( getHype, getMembers, getWeeklyChartList, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyTrackChart
  ) where

#include "group.docs"

import Network.Lastfm.Internal
import Network.Lastfm.XML (xmlWrapper)
import qualified Network.Lastfm.API.Group as API

$(xmlWrapper ["getHype", "getMembers", "getWeeklyChartList", "getWeeklyAlbumChart", "getWeeklyArtistChart", "getWeeklyTrackChart"])

__getHype__
getHype ∷ Group → APIKey → Lastfm Response

__getMembers__
getMembers ∷ Group → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getWeeklyChartList__
getWeeklyChartList ∷ Group → APIKey → Lastfm Response

__getWeeklyAlbumChart__
getWeeklyAlbumChart ∷ Group → Maybe From → Maybe To → APIKey → Lastfm Response

__getWeeklyArtistChart__
getWeeklyArtistChart ∷ Group → Maybe From → Maybe To → APIKey → Lastfm Response

__getWeeklyTrackChart__
getWeeklyTrackChart ∷ Group → Maybe From → Maybe To → APIKey → Lastfm Response
