{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Group
  ( Group(..), Page(..), Limit(..), From(..), To(..)
  , getHype, getMembers, getWeeklyChartList, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyTrackChart
  )  where

import Network.Lastfm.Auth (APIKey)
import Network.Lastfm.Core

newtype Group = Group String deriving (Show, LastfmValue)
newtype Page = Page Int deriving (Show, LastfmValue)
newtype Limit = Limit Int deriving (Show, LastfmValue)
newtype From = From String deriving (Show, LastfmValue)
newtype To = To String deriving (Show, LastfmValue)

getHype :: Group -> APIKey -> IO Response
getHype group apiKey = callAPI "group.getHype"
  [ "group" ?< group
  , "api_key" ?< apiKey
  ]

getMembers :: Group -> Maybe Page -> Maybe Limit -> APIKey -> IO Response
getMembers group page limit apiKey = callAPI "group.getMembers" $
  [ "group" ?< group
  , "api_key" ?< apiKey
  ] ++ optional
    [ "page" ?<< page
    , "limit" ?<< limit
    ]

getWeeklyChartList :: Group -> APIKey -> IO Response
getWeeklyChartList group apiKey = callAPI "group.getWeeklyChartList"
  [ "group" ?< group
  , "api_key" ?< apiKey
  ]

getWeeklyAlbumChart :: Group -> Maybe From -> Maybe To -> APIKey -> IO Response
getWeeklyAlbumChart group from to apiKey = callAPI "group.getWeeklyAlbumChart" $
  [ "group" ?< group
  , "api_key" ?< apiKey
  ] ++ optional
    [ "from" ?<< from
    , "to" ?<< to
    ]

getWeeklyArtistChart :: Group -> Maybe From -> Maybe To -> APIKey -> IO Response
getWeeklyArtistChart group from to apiKey = callAPI "group.getWeeklyArtistChart" $
  [ "group" ?< group
  , "api_key" ?< apiKey
  ] ++ optional
    [ "from" ?<< from
    , "to" ?<< to
    ]


getWeeklyTrackChart :: Group -> Maybe From -> Maybe To -> APIKey -> IO Response
getWeeklyTrackChart group from to apiKey = callAPI "group.getWeeklyTrackChart" $
  [ "group" ?< group
  , "api_key" ?< apiKey
  ] ++ optional
    [ "from" ?<< from
    , "to" ?<< to
    ]
