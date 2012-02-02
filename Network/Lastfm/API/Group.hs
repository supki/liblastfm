module Network.Lastfm.API.Group
  ( getHype, getMembers, getWeeklyChartList, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyTrackChart
  ) where

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), APIKey, From, Group, Limit, Page, To)

getHype :: Group -> APIKey -> Lastfm Response
getHype group apiKey = dispatch $ callAPI "group.getHype"
  [ "group" ?< group
  , "api_key" ?< apiKey
  ]

getMembers :: Group -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getMembers group page limit apiKey = dispatch $ callAPI "group.getMembers"
  [ "group" ?< group
  , "api_key" ?< apiKey
  , "page" ?< page
  , "limit" ?< limit
  ]

getWeeklyChartList :: Group -> APIKey -> Lastfm Response
getWeeklyChartList group apiKey = dispatch $ callAPI "group.getWeeklyChartList"
  [ "group" ?< group
  , "api_key" ?< apiKey
  ]

getWeeklyAlbumChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyAlbumChart group from to apiKey = dispatch $ callAPI "group.getWeeklyAlbumChart"
  [ "group" ?< group
  , "api_key" ?< apiKey
  , "from" ?< from
  , "to" ?< to
  ]

getWeeklyArtistChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyArtistChart group from to apiKey = dispatch $ callAPI "group.getWeeklyArtistChart"
  [ "group" ?< group
  , "api_key" ?< apiKey
  , "from" ?< from
  , "to" ?< to
  ]


getWeeklyTrackChart :: Group -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyTrackChart group from to apiKey = dispatch $ callAPI "group.getWeeklyTrackChart"
  [ "group" ?< group
  , "api_key" ?< apiKey
  , "from" ?< from
  , "to" ?< to
  ]
