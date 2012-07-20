module Network.Lastfm.API.Geo
  ( getEvents, getMetroArtistChart, getMetroHypeArtistChart, getMetroHypeTrackChart
  , getMetroTrackChart, getMetroUniqueArtistChart, getMetroUniqueTrackChart
  , getMetroWeeklyChartlist, getMetros, getTopArtists, getTopTracks
  ) where

import Network.Lastfm.Internal

getEvents ∷ Format
          → Maybe Latitude
          → Maybe Longitude
          → Maybe Location
          → Maybe Distance
          → Maybe Page
          → Maybe Limit
          → APIKey
          → Lastfm Response
getEvents t latitude longitude location distance page limit apiKey = callAPI t
  [ (#) (Method "geo.getEvents")
  , (#) latitude
  , (#) longitude
  , (#) location
  , (#) distance
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getMetroArtistChart ∷ Format → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroArtistChart = getMetroChart "geo.getMetroArtistChart"

getMetroHypeArtistChart ∷ Format → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroHypeArtistChart = getMetroChart "geo.getMetroHypeArtistChart"

getMetroHypeTrackChart ∷ Format → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroHypeTrackChart = getMetroChart "geo.getMetroHypeTrackChart"

getMetroTrackChart ∷ Format → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroTrackChart = getMetroChart "geo.getMetroTrackChart"

getMetroUniqueArtistChart ∷ Format → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroUniqueArtistChart = getMetroChart "geo.getMetroUniqueArtistChart"

getMetroUniqueTrackChart ∷ Format → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroUniqueTrackChart = getMetroChart "geo.getMetroUniqueTrackChart"

getMetroWeeklyChartlist ∷ Format → Metro → APIKey → Lastfm Response
getMetroWeeklyChartlist t metro apiKey = callAPI t
  [ (#) (Method "geo.getMetroWeeklyChartlist")
  , (#) metro
  , (#) apiKey
  ]

getMetros ∷ Format → Maybe Country → APIKey → Lastfm Response
getMetros t country apiKey = callAPI t
  [ (#) (Method "geo.getMetros")
  , (#) country
  , (#) apiKey
  ]

getTopArtists ∷ Format → Country → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists t country page limit apiKey = callAPI t
  [ (#) (Method "geo.getTopArtists")
  , (#) country
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopTracks ∷ Format → Country → Maybe Location → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks t country location page limit apiKey = callAPI t
  [ (#) (Method "geo.getTopTracks")
  , (#) country
  , (#) location
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getMetroChart ∷ String → Format → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroChart method t country metro start end apiKey = callAPI t
  [ (#) (Method method)
  , (#) country
  , (#) metro
  , (#) start
  , (#) end
  , (#) apiKey
  ]
