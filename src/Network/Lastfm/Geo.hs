{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm geo API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Geo as Geo
-- @
module Network.Lastfm.Geo
  ( getEvents, getMetroArtistChart, getMetroHypeArtistChart
  , getMetroHypeTrackChart, getMetroTrackChart, getMetroUniqueArtistChart
  , getMetroUniqueTrackChart, getMetroWeeklyChartlist, getMetros
  , getTopArtists, getTopTracks
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Get all events in a specific location by country or city name.
--
-- Optional: 'longitude', 'latitude', 'location', 'distance', 'page', 'tag', 'festivalsonly', 'limit'
--
-- <http://www.last.fm/api/show/geo.getEvents>
getEvents ∷ Request Ready f
getEvents = api "geo.getEvents"


-- | Get a chart of artists for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroArtistChart>
getMetroArtistChart ∷ Metro → Country → Request Ready f
getMetroArtistChart m c = api "geo.getMetroArtistChart" <> metro m <> country c


-- | Get a chart of hyped (up and coming) artists for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroHypeArtistChart>
getMetroHypeArtistChart ∷ Metro → Country → Request Ready f
getMetroHypeArtistChart m c = api "geo.getMetroHypeArtistChart" <> metro m <> country c


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroHypeTrackChart>
getMetroHypeTrackChart ∷ Metro → Country → Request Ready f
getMetroHypeTrackChart m c = api "geo.getMetroHypeTrackChart" <> metro m <> country c


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroTrackChart>
getMetroTrackChart ∷ Metro → Country → Request Ready f
getMetroTrackChart m c = api "geo.getMetroTrackChart" <> metro m <> country c


-- | Get a chart of the artists which make that metro unique
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroUniqueArtistChart>
getMetroUniqueArtistChart ∷ Metro → Country → Request Ready f
getMetroUniqueArtistChart m c = api "geo.getMetroUniqueArtistChart" <> metro m <> country c


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroUniqueTrackChart>
getMetroUniqueTrackChart ∷ Metro → Country → Request Ready f
getMetroUniqueTrackChart m c = api "geo.getMetroUniqueTrackChart" <> metro m <> country c


-- | Get a list of available chart periods for this metro,
-- expressed as date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/geo.getMetroWeeklyChartlist>
getMetroWeeklyChartlist ∷ Metro → Request Ready f
getMetroWeeklyChartlist m = api "geo.getMetroWeeklyChartlist" <> metro m


-- | Get a list of valid countries and metros for use in the other webservices
--
-- Optional: 'country'
--
-- <http://www.last.fm/api/show/geo.getMetros>
getMetros ∷ Request Ready f
getMetros = api "geo.getMetros"


-- | Get the most popular artists on Last.fm by country
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/geo.getTopArtists>
getTopArtists ∷ Country → Request Ready f
getTopArtists c = api "geo.getTopArtists" <> country c


-- | Get the most popular tracks on Last.fm last week by country
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/geo.getTopTracks>
getTopTracks ∷ Country → Request Ready f
getTopTracks c = api "geo.getTopTracks" <> country c
