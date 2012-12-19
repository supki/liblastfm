{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm tag API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Tag as Tag
-- @
module Network.Lastfm.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

import Network.Lastfm.Request


-- | Get the metadata for a tag
--
-- Optional: language
--
-- <http://www.last.fm/api/show/tag.getInfo>
getInfo ∷ Request f Send (Tag → APIKey → Ready)
getInfo = api "tag.getInfo"


-- | Search for tags similar to this one. Returns tags ranked by similarity, based on listening data.
--
-- <http://www.last.fm/api/show/tag.getSimilar>
getSimilar ∷ Request f Send (Tag → APIKey → Ready)
getSimilar = api "tag.getSimilar"


-- | Get the top albums tagged by this tag, ordered by tag count.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.getTopAlbums>
getTopAlbums ∷ Request f Send (Tag → APIKey → Ready)
getTopAlbums = api "tag.getTopAlbums"


-- | Get the top artists tagged by this tag, ordered by tag count.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.getTopArtists>
getTopArtists ∷ Request f Send (Tag → APIKey → Ready)
getTopArtists = api "tag.getTopArtists"


-- | Fetches the top global tags on Last.fm, sorted by popularity (number of times used)
--
-- <http://www.last.fm/api/show/tag.getTopTags>
getTopTags ∷ Request f Send (APIKey → Ready)
getTopTags = api "tag.getTopTags"


-- | Get the top tracks tagged by this tag, ordered by tag count.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.getTopTracks>
getTopTracks ∷ Request f Send (Tag → APIKey → Ready)
getTopTracks = api "tag.getTopTracks"


-- | Get an artist chart for a tag, for a given date range.
-- If no date range is supplied, it will return the most recent artist chart for this tag.
--
-- Optional: 'from', 'to', 'limit'
--
-- <http://www.last.fm/api/show/tag.getWeeklyArtistChart>
getWeeklyArtistChart ∷ Request f Send (Tag → APIKey → Ready)
getWeeklyArtistChart = api "tag.getWeeklyArtistChart"


-- | Get a list of available charts for this tag, expressed as
-- date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/tag.getWeeklyChartList>
getWeeklyChartList ∷ Request f Send (Tag → APIKey → Ready)
getWeeklyChartList = api "tag.getWeeklyChartList"


-- | Search for a tag by name. Returns matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.search>
search ∷ Request f Send (Tag → APIKey → Ready)
search = api "tag.search"
