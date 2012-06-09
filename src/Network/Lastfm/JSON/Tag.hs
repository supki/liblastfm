{-# LANGUAGE TemplateHaskell #-}
-- | Tag API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Tag as API

$(json ["getInfo", "getSimilar", "getTopAlbums", "getTopArtists", "getTopTags", "getTopTracks", "getWeeklyArtistChart", "getWeeklyChartList", "search"])

-- | Get the metadata for a tag.
--
-- More: <http://www.last.fm/api/show/tag.getInfo>
getInfo ∷ Tag → Maybe Language → APIKey → Lastfm Response

-- | Search for tags similar to this one. Returns tags ranked by similarity, based on listening data.
--
-- More: <http://www.last.fm/api/show/tag.getSimilar>
getSimilar ∷ Tag → APIKey → Lastfm Response

-- | Get the top albums tagged by this tag, ordered by tag count.
--
-- More: <http://www.last.fm/api/show/tag.getTopAlbums>
getTopAlbums ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the top artists tagged by this tag, ordered by tag count.
--
-- More: <http://www.last.fm/api/show/tag.getTopArtists>
getTopArtists ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Fetches the top global tags on Last.fm, sorted by popularity (number of times used).
--
-- More: <http://www.last.fm/api/show/tag.getTopTags>
getTopTags ∷ APIKey → Lastfm Response

-- | Get the top tracks tagged by this tag, ordered by tag count.
--
-- More: <http://www.last.fm/api/show/tag.getTopTracks>
getTopTracks ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get an artist chart for a tag, for a given date range. If no date range is supplied, it will return the most recent artist chart for this tag.
--
-- More: <http://www.last.fm/api/show/tag.getWeeklyArtistChart>
getWeeklyArtistChart ∷ Tag → Maybe From → Maybe To → Maybe Limit → APIKey → Lastfm Response

-- | Get a list of available charts for this tag, expressed as date ranges which can be sent to the chart services.
--
-- More: <http://www.last.fm/api/show/tag.getWeeklyChartList>
getWeeklyChartList ∷ Tag → APIKey → Lastfm Response

-- | Search for a tag by name. Returns matches sorted by relevance.
--
-- More: <http://www.last.fm/api/show/tag.search>
search ∷ Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
