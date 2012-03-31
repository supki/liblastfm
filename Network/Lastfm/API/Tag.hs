-- | Tag API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

import Control.Monad.Error (runErrorT)
import Network.Lastfm

-- | Get the metadata for a tag.
--
-- More: <http://www.lastfm.ru/api/show/tag.getInfo>
getInfo :: Tag -> Maybe Language -> APIKey -> Lastfm Response
getInfo tag language apiKey = callAPI
  [ (#) (Method "tag.getInfo")
  , (#) tag
  , (#) language
  , (#) apiKey
  ]

-- | Search for tags similar to this one. Returns tags ranked by similarity, based on listening data.
--
-- More: <http://www.lastfm.ru/api/show/tag.getSimilar>
getSimilar :: Tag -> APIKey -> Lastfm Response
getSimilar tag apiKey = callAPI
  [ (#) (Method "tag.getSimilar")
  , (#) tag
  , (#) apiKey
  ]

-- | Get the top albums tagged by this tag, ordered by tag count.
--
-- More: <http://www.lastfm.ru/api/show/tag.getTopAlbums>
getTopAlbums :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopAlbums tag page limit apiKey = callAPI
  [ (#) (Method "tag.getTopAlbums")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get the top artists tagged by this tag, ordered by tag count.
--
-- More: <http://www.lastfm.ru/api/show/tag.getTopArtists>
getTopArtists :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopArtists tag limit page apiKey = callAPI
  [ (#) (Method "tag.getTopArtists")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Fetches the top global tags on Last.fm, sorted by popularity (number of times used).
--
-- More: <http://www.lastfm.ru/api/show/tag.getTopTags>
getTopTags :: APIKey -> Lastfm Response
getTopTags apiKey = callAPI
  [ (#) (Method "tag.getTopArtists")
  , (#) apiKey
  ]

-- | Get the top tracks tagged by this tag, ordered by tag count.
--
-- More: <http://www.lastfm.ru/api/show/tag.getTopTracks>
getTopTracks :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks tag limit page apiKey = callAPI
  [ (#) (Method "tag.getTopTracks")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get an artist chart for a tag, for a given date range. If no date range is supplied, it will return the most recent artist chart for this tag.
--
-- More: <http://www.lastfm.ru/api/show/tag.getWeeklyArtistChart>
getWeeklyArtistChart :: Tag -> Maybe From -> Maybe To -> Maybe Limit -> APIKey -> Lastfm Response
getWeeklyArtistChart tag from to limit apiKey = callAPI
  [ (#) (Method "tag.getWeeklyArtistChart")
  , (#) tag
  , (#) from
  , (#) to
  , (#) limit
  , (#) apiKey
  ]

-- | Get a list of available charts for this tag, expressed as date ranges which can be sent to the chart services.
--
-- More: <http://www.lastfm.ru/api/show/tag.getWeeklyChartList>
getWeeklyChartList :: Tag -> APIKey -> Lastfm Response
getWeeklyChartList tag apiKey = callAPI
  [ (#) (Method "tag.getWeeklyChartList")
  , (#) tag
  , (#) apiKey
  ]

-- | Search for a tag by name. Returns matches sorted by relevance.
--
-- More: <http://www.lastfm.ru/api/show/tag.search>
search :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search tag page limit apiKey = callAPI
  [ (#) (Method "tag.search")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]
