{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Network.Lastfm.Request


-- | Get all events in a specific location by country or city name.
--
-- Optional: 'longitude', 'latitude', 'location', 'distance', 'page', 'tag', 'festivalsonly', 'limit'
--
-- <http://www.last.fm/api/show/geo.getEvents>
getEvents :: Request f (APIKey -> Ready)
getEvents = api "geo.getEvents"


-- | Get a chart of artists for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroArtistChart>
getMetroArtistChart :: Request f (Metro -> Country -> APIKey -> Ready)
getMetroArtistChart = api "geo.getMetroArtistChart"


-- | Get a chart of hyped (up and coming) artists for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroHypeArtistChart>
getMetroHypeArtistChart :: Request f (Metro -> Country -> APIKey -> Ready)
getMetroHypeArtistChart = api "geo.getMetroHypeArtistChart"


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroHypeTrackChart>
getMetroHypeTrackChart :: Request f (Metro -> Country -> APIKey -> Ready)
getMetroHypeTrackChart = api "geo.getMetroHypeTrackChart"


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroTrackChart>
getMetroTrackChart :: Request f (Metro -> Country -> APIKey -> Ready)
getMetroTrackChart = api "geo.getMetroTrackChart"


-- | Get a chart of the artists which make that metro unique
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroUniqueArtistChart>
getMetroUniqueArtistChart :: Request f (Metro -> Country -> APIKey -> Ready)
getMetroUniqueArtistChart = api "geo.getMetroUniqueArtistChart"


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroUniqueTrackChart>
getMetroUniqueTrackChart :: Request f (Metro -> Country -> APIKey -> Ready)
getMetroUniqueTrackChart = api "geo.getMetroUniqueTrackChart"


-- | Get a list of available chart periods for this metro,
-- expressed as date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/geo.getMetroWeeklyChartlist>
getMetroWeeklyChartlist :: Request f (Metro -> APIKey -> Ready)
getMetroWeeklyChartlist = api "geo.getMetroWeeklyChartlist"


-- | Get a list of valid countries and metros for use in the other webservices
--
-- Optional: 'country'
--
-- <http://www.last.fm/api/show/geo.getMetros>
getMetros :: Request f (APIKey -> Ready)
getMetros = api "geo.getMetros"


-- | Get the most popular artists on Last.fm by country
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/geo.getTopArtists>
getTopArtists :: Request f (Country -> APIKey -> Ready)
getTopArtists = api "geo.getTopArtists"


-- | Get the most popular tracks on Last.fm last week by country
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/geo.getTopTracks>
getTopTracks :: Request f (Country -> APIKey -> Ready)
getTopTracks = api "geo.getTopTracks"
