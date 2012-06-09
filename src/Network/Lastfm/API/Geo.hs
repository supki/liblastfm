module Network.Lastfm.API.Geo
  ( getEvents, getMetroArtistChart, getMetroHypeArtistChart, getMetroHypeTrackChart
  , getMetroTrackChart, getMetroUniqueArtistChart, getMetroUniqueTrackChart
  , getMetroWeeklyChartlist, getMetros, getTopArtists, getTopTracks
  ) where

import Network.Lastfm.Internal

getEvents ∷ ResponseType
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

getMetroArtistChart ∷ ResponseType → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroArtistChart = getMetroChart "geo.getMetroArtistChart"

getMetroHypeArtistChart ∷ ResponseType → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroHypeArtistChart = getMetroChart "geo.getMetroHypeArtistChart"

getMetroHypeTrackChart ∷ ResponseType → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroHypeTrackChart = getMetroChart "geo.getMetroHypeTrackChart"

getMetroTrackChart ∷ ResponseType → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroTrackChart = getMetroChart "geo.getMetroTrackChart"

getMetroUniqueArtistChart ∷ ResponseType → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroUniqueArtistChart = getMetroChart "geo.getMetroUniqueArtistChart"

getMetroUniqueTrackChart ∷ ResponseType → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroUniqueTrackChart = getMetroChart "geo.getMetroUniqueTrackChart"

getMetroWeeklyChartlist ∷ ResponseType → Metro → APIKey → Lastfm Response
getMetroWeeklyChartlist t metro apiKey = callAPI t
  [ (#) (Method "geo.getMetroWeeklyChartlist")
  , (#) metro
  , (#) apiKey
  ]

getMetros ∷ ResponseType → Maybe Country → APIKey → Lastfm Response
getMetros t country apiKey = callAPI t
  [ (#) (Method "geo.getMetros")
  , (#) country
  , (#) apiKey
  ]

getTopArtists ∷ ResponseType → Country → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists t country page limit apiKey = callAPI t
  [ (#) (Method "geo.getTopArtists")
  , (#) country
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopTracks ∷ ResponseType → Country → Maybe Location → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks t country location page limit apiKey = callAPI t
  [ (#) (Method "geo.getTopTracks")
  , (#) country
  , (#) location
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getMetroChart ∷ String → ResponseType → Country → Metro → Maybe Start → Maybe End → APIKey → Lastfm Response
getMetroChart method t country metro start end apiKey = callAPI t
  [ (#) (Method method)
  , (#) country
  , (#) metro
  , (#) start
  , (#) end
  , (#) apiKey
  ]
