-- | Group API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Group
  ( getHype, getMembers, getWeeklyChartList, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyTrackChart
  ) where

import Network.Lastfm

-- | Get the hype list for a group.
--
-- More: <http://www.lastfm.ru/api/show/group.getHype>
getHype :: Group -> APIKey -> Lastfm Response
getHype group apiKey = callAPI
  [ (#) (Method "group.getHype")
  , (#) group
  , (#) apiKey
  ]

-- | Get a list of members for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getMembers>
getMembers :: Group -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getMembers group page limit apiKey = callAPI
  [ (#) (Method "group.getMembers")
  , (#) group
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get an album chart for a group, for a given date range. If no date range is supplied, it will return the most recent album chart for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyAlbumChart>
getWeeklyChartList :: Group -> APIKey -> Lastfm Response
getWeeklyChartList group apiKey = callAPI
  [ (#) (Method "group.getWeeklyChartList")
  , (#) group
  , (#) apiKey
  ]

-- | Get an artist chart for a group, for a given date range. If no date range is supplied, it will return the most recent artist chart for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyArtistChart>
getWeeklyAlbumChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyAlbumChart group from to apiKey = callAPI
  [ (#) (Method "group.getWeeklyAlbumChart")
  , (#) group
  , (#) from
  , (#) to
  , (#) apiKey
  ]

-- | Get a list of available charts for this group, expressed as date ranges which can be sent to the chart services.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyChartList>
getWeeklyArtistChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyArtistChart group from to apiKey = callAPI
  [ (#) (Method "group.getWeeklyArtistChart")
  , (#) group
  , (#) from
  , (#) to
  , (#) apiKey
  ]


-- | Get a track chart for a group, for a given date range. If no date range is supplied, it will return the most recent track chart for this group.
--
-- More: <http://www.lastfm.ru/api/show/group.getWeeklyTrackChart>
getWeeklyTrackChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyTrackChart group from to apiKey = callAPI
  [ (#) (Method "group.getWeeklyTrackChart")
  , (#) group
  , (#) from
  , (#) to
  , (#) apiKey
  ]
