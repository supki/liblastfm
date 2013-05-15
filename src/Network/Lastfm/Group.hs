{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Network.Lastfm.Request


-- | Get the hype list for a group
--
-- <http://www.last.fm/api/show/group.getHype>
getHype :: Request f Send (Group -> APIKey -> Ready)
getHype = api "group.getHype"
{-# INLINE getHype #-}


-- | Get a list of members for this group.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/group.getMembers>
getMembers :: Request f Send (Group -> APIKey -> Ready)
getMembers = api "group.getMembers"
{-# INLINE getMembers #-}


-- | Get an album chart for a group, for a given date range.
-- If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyAlbumChart>
getWeeklyAlbumChart :: Request f Send (Group -> APIKey -> Ready)
getWeeklyAlbumChart = api "group.getWeeklyAlbumChart"
{-# INLINE getWeeklyAlbumChart #-}


-- | Get an artist chart for a group, for a given date range.
-- If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyArtistChart>
getWeeklyArtistChart :: Request f Send (Group -> APIKey -> Ready)
getWeeklyArtistChart = api "group.getWeeklyArtistChart"
{-# INLINE getWeeklyArtistChart #-}


-- | Get a list of available charts for this group, expressed as
-- date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/group.getWeeklyChartList>
getWeeklyChartList :: Request f Send (Group -> APIKey -> Ready)
getWeeklyChartList = api "group.getWeeklyChartList"
{-# INLINE getWeeklyChartList #-}


-- | Get a track chart for a group, for a given date range.
-- If no date range is supplied, it will return the most recent album chart for this group.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/group.getWeeklyTrackChart>
getWeeklyTrackChart :: Request f Send (Group -> APIKey -> Ready)
getWeeklyTrackChart = api "group.getWeeklyTrackChart"
{-# INLINE getWeeklyTrackChart #-}
