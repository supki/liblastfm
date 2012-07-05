{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Geo API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Geo
  ( getEvents, getMetroArtistChart, getMetroHypeArtistChart, getMetroHypeTrackChart
  , getMetroTrackChart, getMetroUniqueArtistChart, getMetroUniqueTrackChart
  , getMetroWeeklyChartlist, getMetros, getTopArtists, getTopTracks
  ) where

#include "geo.docs"

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Geo as API

$(json ["getEvents", "getMetroArtistChart", "getMetroHypeArtistChart", "getMetroHypeTrackChart", "getMetroTrackChart", "getMetroUniqueArtistChart", "getMetroUniqueTrackChart", "getMetroWeeklyChartlist", "getMetros", "getTopArtists", "getTopTracks"])

__getEvents__
getEvents ∷ Maybe Latitude
          → Maybe Longitude
          → Maybe Location
          → Maybe Distance
          → Maybe Page
          → Maybe Limit
          → APIKey
          → Lastfm Response

__getMetroArtistChart__
getMetroArtistChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

__getMetroHypeArtistChart__
getMetroHypeArtistChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

__getMetroHypeTrackChart__
getMetroHypeTrackChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

__getMetroTrackChart__
getMetroTrackChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

__getMetroUniqueArtistChart__
getMetroUniqueArtistChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

__getMetroUniqueTrackChart__
getMetroUniqueTrackChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

__getMetroWeeklyChartlist__
getMetroWeeklyChartlist ∷ Metro → APIKey → Lastfm Response

__getMetros__
getMetros ∷ Maybe Country → APIKey → Lastfm Response

__getTopArtists__
getTopArtists ∷ Country → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopTracks__
getTopTracks ∷ Country → Maybe Location → Maybe Page → Maybe Limit → APIKey → Lastfm Response
