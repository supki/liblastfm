{-# LANGUAGE TemplateHaskell #-}
-- | Geo API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Geo
  ( getEvents, getMetroArtistChart, getMetroHypeArtistChart, getMetroHypeTrackChart
  , getMetroTrackChart, getMetroUniqueArtistChart, getMetroUniqueTrackChart
  , getMetroWeeklyChartlist, getMetros, getTopArtists, getTopTracks
  ) where

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Geo as API

$(xml ["getEvents", "getMetroArtistChart", "getMetroHypeArtistChart", "getMetroHypeTrackChart", "getMetroTrackChart", "getMetroUniqueArtistChart", "getMetroUniqueTrackChart", "getMetroWeeklyChartlist", "getMetros", "getTopArtists", "getTopTracks"])

-- | Get all events in a specific location by country or city name.
--
-- More: <http://www.last.fm/api/show/geo.getEvents>
getEvents ∷ Maybe Latitude
          → Maybe Longitude
          → Maybe Location
          → Maybe Distance
          → Maybe Page
          → Maybe Limit
          → APIKey
          → Lastfm Response

-- | Get a chart of artists for a metro.
--
-- More: <http://www.last.fm/api/show/geo.getMetroArtistChart>
getMetroArtistChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

-- | Get a chart of hyped (up and coming) artists for a metro.
--
-- More: <http://www.last.fm/api/show/geo.getMetroHypeArtistChart>
getMetroHypeArtistChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

-- | Get a chart of hyped (up and coming) tracks for a metro.
--
-- More: <http://www.last.fm/api/show/geo.getMetroHypeTrackChart>
getMetroHypeTrackChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

-- | Get a chart of tracks for a metro.
--
-- More: <http://www.last.fm/api/show/geo.getMetroTrackChart>
getMetroTrackChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

-- | Get a chart of the artists which make that metro unique.
--
-- More: <http://www.last.fm/api/show/geo.getMetroUniqueArtistChart>
getMetroUniqueArtistChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

-- | Get a chart of the tracks which make that metro unique.
--
-- More: <http://www.last.fm/api/show/geo.getMetroUniqueTrackChart>
getMetroUniqueTrackChart ∷ Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response

-- | Get a list of available chart periods for this metro, expressed as date ranges which can be sent to the chart services.
--
-- More: <http://www.last.fm/api/show/geo.getMetroWeeklyChartlist>
getMetroWeeklyChartlist ∷ Metro → APIKey → Lastfm Response

-- | Get a list of valid countries and metros for use in the other webservices.
--
-- More: <http://www.last.fm/api/show/geo.getMetros>
getMetros ∷ Maybe Country → APIKey → Lastfm Response

-- | Get the most popular artists on Last.fm by country.
--
-- More: <http://www.last.fm/api/show/geo.getTopArtists>
getTopArtists ∷ Country → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the most popular tracks on Last.fm last week by country.
--
-- More: <http://www.last.fm/api/show/geo.getTopTracks>
getTopTracks ∷ Country → Maybe Location → Maybe Page → Maybe Limit → APIKey → Lastfm Response
