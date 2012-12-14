{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm group API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Group as Group
-- @
module Network.Lastfm.Group
  ( getHype, getMembers, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyChartList, getWeeklyTrackChart
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Get the hype list for a group
--
-- <http://www.last.fm/api/show/group.getHype>
getHype ∷ Group → Request f Ready
getHype g = api "group.getHype" <> group g


-- | Get a list of members for this group.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/group.getMembers>
getMembers ∷ Group → Request f Ready
getMembers g = api "group.getMembers" <> group g


-- | Get an album chart for a group, for a given date range. If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyAlbumChart>
getWeeklyAlbumChart ∷ Group → Request f Ready
getWeeklyAlbumChart g = api "group.getWeeklyAlbumChart" <> group g


-- | Get an artist chart for a group, for a given date range. If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyArtistChart>
getWeeklyArtistChart ∷ Group → Request f Ready
getWeeklyArtistChart g = api "group.getWeeklyArtistChart" <> group g


-- | Get a list of available charts for this group, expressed as date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/group.getWeeklyChartList>
getWeeklyChartList ∷ Group → Request f Ready
getWeeklyChartList g = api "group.getWeeklyChartList" <> group g


-- | Get a track chart for a group, for a given date range. If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyTrackChart>
getWeeklyTrackChart ∷ Group → Request f Ready
getWeeklyTrackChart g = api "group.getWeeklyTrackChart" <> group g

