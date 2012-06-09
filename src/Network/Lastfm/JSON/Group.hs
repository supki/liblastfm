{-# LANGUAGE TemplateHaskell #-}
-- | Group API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Group
  ( getHype, getMembers, getWeeklyChartList, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyTrackChart
  ) where

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Group as API

$(json ["getHype", "getMembers", "getWeeklyChartList", "getWeeklyAlbumChart", "getWeeklyArtistChart", "getWeeklyTrackChart"])

-- | Get the hype list for a group.
--
-- More: <http://www.last.fm/api/show/group.getHype>
getHype ∷ Group → APIKey → Lastfm Response

-- | Get a list of members for this group.
--
-- More: <http://www.last.fm/api/show/group.getMembers>
getMembers ∷ Group → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get an album chart for a group, for a given date range. If no date range is supplied, it will return the most recent album chart for this group.
--
-- More: <http://www.last.fm/api/show/group.getWeeklyAlbumChart>
getWeeklyChartList ∷ Group → APIKey → Lastfm Response

-- | Get an artist chart for a group, for a given date range. If no date range is supplied, it will return the most recent artist chart for this group.
--
-- More: <http://www.last.fm/api/show/group.getWeeklyArtistChart>
getWeeklyAlbumChart ∷ Group → Maybe From → Maybe To → APIKey → Lastfm Response

-- | Get a list of available charts for this group, expressed as date ranges which can be sent to the chart services.
--
-- More: <http://www.last.fm/api/show/group.getWeeklyChartList>
getWeeklyArtistChart ∷ Group → Maybe From → Maybe To → APIKey → Lastfm Response


-- | Get a track chart for a group, for a given date range. If no date range is supplied, it will return the most recent track chart for this group.
--
-- More: <http://www.last.fm/api/show/group.getWeeklyTrackChart>
getWeeklyTrackChart ∷ Group → Maybe From → Maybe To → APIKey → Lastfm Response
