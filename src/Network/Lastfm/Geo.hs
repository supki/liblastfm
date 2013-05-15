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
getEvents :: Request f Send (APIKey -> Ready)
getEvents = api "geo.getEvents"
{-# INLINE getEvents #-}


-- | Get a chart of artists for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroArtistChart>
getMetroArtistChart :: Request f Send (Metro -> Country -> APIKey -> Ready)
getMetroArtistChart = api "geo.getMetroArtistChart"
{-# INLINE getMetroArtistChart #-}


-- | Get a chart of hyped (up and coming) artists for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroHypeArtistChart>
getMetroHypeArtistChart :: Request f Send (Metro -> Country -> APIKey -> Ready)
getMetroHypeArtistChart = api "geo.getMetroHypeArtistChart"
{-# INLINE getMetroHypeArtistChart #-}


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroHypeTrackChart>
getMetroHypeTrackChart :: Request f Send (Metro -> Country -> APIKey -> Ready)
getMetroHypeTrackChart = api "geo.getMetroHypeTrackChart"
{-# INLINE getMetroHypeTrackChart #-}


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroTrackChart>
getMetroTrackChart :: Request f Send (Metro -> Country -> APIKey -> Ready)
getMetroTrackChart = api "geo.getMetroTrackChart"
{-# INLINE getMetroTrackChart #-}


-- | Get a chart of the artists which make that metro unique
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroUniqueArtistChart>
getMetroUniqueArtistChart :: Request f Send (Metro -> Country -> APIKey -> Ready)
getMetroUniqueArtistChart = api "geo.getMetroUniqueArtistChart"
{-# INLINE getMetroUniqueArtistChart #-}


-- | Get a chart of tracks for a metro
--
-- Optional: 'start', 'end', 'page', 'limit'
--
-- <http://www.last.fm/api/show/geo.getMetroUniqueTrackChart>
getMetroUniqueTrackChart :: Request f Send (Metro -> Country -> APIKey -> Ready)
getMetroUniqueTrackChart = api "geo.getMetroUniqueTrackChart"
{-# INLINE getMetroUniqueTrackChart #-}


-- | Get a list of available chart periods for this metro,
-- expressed as date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/geo.getMetroWeeklyChartlist>
getMetroWeeklyChartlist :: Request f Send (Metro -> APIKey -> Ready)
getMetroWeeklyChartlist = api "geo.getMetroWeeklyChartlist"
{-# INLINE getMetroWeeklyChartlist #-}


-- | Get a list of valid countries and metros for use in the other webservices
--
-- Optional: 'country'
--
-- <http://www.last.fm/api/show/geo.getMetros>
getMetros :: Request f Send (APIKey -> Ready)
getMetros = api "geo.getMetros"
{-# INLINE getMetros #-}


-- | Get the most popular artists on Last.fm by country
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/geo.getTopArtists>
getTopArtists :: Request f Send (Country -> APIKey -> Ready)
getTopArtists = api "geo.getTopArtists"
{-# INLINE getTopArtists #-}


-- | Get the most popular tracks on Last.fm last week by country
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/geo.getTopTracks>
getTopTracks :: Request f Send (Country -> APIKey -> Ready)
getTopTracks = api "geo.getTopTracks"
{-# INLINE getTopTracks #-}
