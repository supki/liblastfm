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

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Get the metadata for a tag
--
-- Optional: language
--
-- <http://www.last.fm/api/show/tag.getInfo>
getInfo ∷ Tag → Request Ready f
getInfo t = api "tag.getInfo" <> tag t


-- | Search for tags similar to this one. Returns tags ranked by similarity, based on listening data.
--
-- <http://www.last.fm/api/show/tag.getSimilar>
getSimilar ∷ Tag → Request Ready f
getSimilar t = api "tag.getSimilar" <> tag t


-- | Get the top albums tagged by this tag, ordered by tag count.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.getTopAlbums>
getTopAlbums ∷ Tag → Request Ready f
getTopAlbums t = api "tag.getTopAlbums" <> tag t


-- | Get the top artists tagged by this tag, ordered by tag count.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.getTopArtists>
getTopArtists ∷ Tag → Request Ready f
getTopArtists t = api "tag.getTopArtists" <> tag t


-- | Fetches the top global tags on Last.fm, sorted by popularity (number of times used)
--
-- <http://www.last.fm/api/show/tag.getTopTags>
getTopTags ∷ Request Ready f
getTopTags = api "tag.getTopTags"


-- | Get the top tracks tagged by this tag, ordered by tag count.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.getTopTracks>
getTopTracks ∷ Tag → Request Ready f
getTopTracks t = api "tag.getTopTracks" <> tag t


-- | Get an artist chart for a tag, for a given date range.
-- If no date range is supplied, it will return the most recent artist chart for this tag.
--
-- Optional: 'from', 'to', 'limit'
--
-- <http://www.last.fm/api/show/tag.getWeeklyArtistChart>
getWeeklyArtistChart ∷ Tag → Request Ready f
getWeeklyArtistChart t = api "tag.getWeeklyArtistChart" <> tag t


-- | Get a list of available charts for this tag, expressed as
-- date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/tag.getWeeklyChartList>
getWeeklyChartList ∷ Tag → Request Ready f
getWeeklyChartList t = api "tag.getWeeklyChartList" <> tag t


-- | Search for a tag by name. Returns matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/tag.search>
search ∷ Tag → Request Ready f
search t = api "tag.search" <> tag t
