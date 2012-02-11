-- | Group API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Group
  ( getHype, getMembers, getWeeklyChartList, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyTrackChart
  ) where

import Network.Lastfm.Response
import Network.Lastfm.Types ((?<), APIKey, From, Group, Limit, Page, To)

-- | Get the hype list for a group.
--
-- More: <http://www.lastfm.ru/api/show/group.getHype>
getHype :: Group -> APIKey -> Lastfm Response
getHype group apiKey = dispatch $ callAPI "group.getHype"
  [ "group" ?< group
  , "api_key" ?< apiKey
  ]

-- | Get a list of members for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getMembers>
getMembers :: Group -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getMembers group page limit apiKey = dispatch $ callAPI "group.getMembers"
  [ "group" ?< group
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Get an album chart for a group, for a given date range. If no date range is supplied, it will return the most recent album chart for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyAlbumChart>
getWeeklyChartList :: Group -> APIKey -> Lastfm Response
getWeeklyChartList group apiKey = dispatch $ callAPI "group.getWeeklyChartList" ["group" ?< group, "api_key" ?< apiKey]

-- | Get an artist chart for a group, for a given date range. If no date range is supplied, it will return the most recent artist chart for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyArtistChart>
getWeeklyAlbumChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyAlbumChart group from to apiKey = dispatch $ callAPI "group.getWeeklyAlbumChart"
  [ "group" ?< group
  , "api_key" ?< apiKey
  , "from" ?< from
  , "to" ?< to
  ]

-- | Get a list of available charts for this group, expressed as date ranges which can be sent to the chart services.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyChartList>
getWeeklyArtistChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyArtistChart group from to apiKey = dispatch $ callAPI "group.getWeeklyArtistChart"
  [ "group" ?< group
  , "api_key" ?< apiKey
  , "from" ?< from
  , "to" ?< to
  ]


-- | Get a track chart for a group, for a given date range. If no date range is supplied, it will return the most recent track chart for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyTrackChart>
getWeeklyTrackChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyTrackChart group from to apiKey = dispatch $ callAPI "group.getWeeklyTrackChart"
  [ "group" ?< group
  , "api_key" ?< apiKey
  , "from" ?< from
  , "to" ?< to
  ]
