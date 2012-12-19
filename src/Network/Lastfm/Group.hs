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

import Data.Void (Void)
import Network.Lastfm.Request


-- | Get the hype list for a group
--
-- <http://www.last.fm/api/show/group.getHype>
getHype ∷ Request f Ready (Group → APIKey → Void)
getHype = api "group.getHype"


-- | Get a list of members for this group.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/group.getMembers>
getMembers ∷ Request f Ready (Group → APIKey → Void)
getMembers = api "group.getMembers"


-- | Get an album chart for a group, for a given date range.
-- If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyAlbumChart>
getWeeklyAlbumChart ∷ Request f Ready (Group → APIKey → Void)
getWeeklyAlbumChart = api "group.getWeeklyAlbumChart"


-- | Get an artist chart for a group, for a given date range.
-- If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyArtistChart>
getWeeklyArtistChart ∷ Request f Ready (Group → APIKey → Void)
getWeeklyArtistChart = api "group.getWeeklyArtistChart"


-- | Get a list of available charts for this group, expressed as
-- date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/group.getWeeklyChartList>
getWeeklyChartList ∷ Request f Ready (Group → APIKey → Void)
getWeeklyChartList = api "group.getWeeklyChartList"


-- | Get a track chart for a group, for a given date range.
-- If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyTrackChart>
getWeeklyTrackChart ∷ Request f Ready (Group → APIKey → Void)
getWeeklyTrackChart = api "group.getWeeklyTrackChart"
