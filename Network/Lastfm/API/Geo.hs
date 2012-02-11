-- | Geo API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Geo
  ( getEvents, getMetroArtistChart, getMetroHypeArtistChart, getMetroHypeTrackChart
  , getMetroTrackChart, getMetroUniqueArtistChart, getMetroUniqueTrackChart
  , getMetroWeeklyChartlist, getMetros, getTopArtists, getTopTracks
  ) where

import Network.Lastfm.Response
import Network.Lastfm.Types ( (?<), APIKey, Country, Distance, From, Latitude
                            , Limit, Location, Longitude, Metro, Page, To
                            )

-- | Get all events in a specific location by country or city name.
--
-- More: <http://www.lastfm.ru/api/show/geo.getEvents>
getEvents :: Maybe Latitude
          -> Maybe Longitude
          -> Maybe Location
          -> Maybe Distance
          -> Maybe Page
          -> Maybe Limit
          -> APIKey
          -> Lastfm Response
getEvents latitude longitude location distance page limit apiKey = dispatch $ callAPI "geo.getEvents"
  [ "lat" ?< latitude
  , "long" ?< longitude
  , "location" ?< location
  , "distance" ?< distance
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Get a chart of artists for a metro.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetroArtistChart>
getMetroArtistChart :: Country -> Metro -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getMetroArtistChart = getMetroChart "geo.getMetroArtistChart"

-- | Get a chart of hyped (up and coming) artists for a metro.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetroHypeArtistChart>
getMetroHypeArtistChart :: Country -> Metro -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getMetroHypeArtistChart = getMetroChart "geo.getMetroHypeArtistChart"

-- | Get a chart of hyped (up and coming) tracks for a metro.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetroHypeTrackChart>
getMetroHypeTrackChart :: Country -> Metro -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getMetroHypeTrackChart = getMetroChart "geo.getMetroHypeTrackChart"

-- | Get a chart of tracks for a metro.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetroTrackChart>
getMetroTrackChart :: Country -> Metro -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getMetroTrackChart = getMetroChart "geo.getMetroTrackChart"

-- | Get a chart of the artists which make that metro unique.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetroUniqueArtistChart>
getMetroUniqueArtistChart :: Country -> Metro -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getMetroUniqueArtistChart = getMetroChart "geo.getMetroUniqueArtistChart"

-- | Get a chart of the tracks which make that metro unique.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetroUniqueTrackChart>
getMetroUniqueTrackChart :: Country -> Metro -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getMetroUniqueTrackChart = getMetroChart "geo.getMetroUniqueTrackChart"

-- | Get a list of available chart periods for this metro, expressed as date ranges which can be sent to the chart services.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetroWeeklyChartlist>
getMetroWeeklyChartlist :: Metro -> APIKey -> Lastfm Response
getMetroWeeklyChartlist metro apiKey = dispatch $ callAPI "geo.getMetroWeeklyChartlist" ["metro" ?< metro, "api_key" ?< apiKey]

-- | Get a list of valid countries and metros for use in the other webservices.
--
-- More: <http://www.lastfm.ru/api/show/geo.getMetros>
getMetros :: Maybe Country -> APIKey -> Lastfm Response
getMetros country apiKey = dispatch $ callAPI "geo.getMetros"
  [ "country" ?< country
  , "api_key" ?< apiKey
  ]

-- | Get the most popular artists on Last.fm by country.
--
-- More: <http://www.lastfm.ru/api/show/geo.getTopArtists>
getTopArtists :: Country -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopArtists country page limit apiKey = dispatch $ callAPI "geo.getTopArtists"
  [ "country" ?< country
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Get the most popular tracks on Last.fm last week by country.
--
-- More: <http://www.lastfm.ru/api/show/geo.getTopTracks>
getTopTracks :: Country -> Maybe Location -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks country location page limit apiKey = dispatch $ callAPI "geo.getTopTracks"
  [ "country" ?< country
  , "location" ?< location
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getMetroChart :: String -> Country -> Metro -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getMetroChart method country metro from to apiKey = dispatch $ callAPI method
  [ "country" ?< country
  , "metro" ?< metro
  , "start" ?< from
  , "end" ?< to
  , "api_key" ?< apiKey
  ]
