module Network.Lastfm.User
  ( getArtistTracks, getBannedTracks, getEvents, getFriends, getInfo, getLovedTracks
  ) where

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), APIKey, Artist, FestivalsOnly, From, Limit, Page, RecentTracks, To, User)

getArtistTracks :: User
                -> Artist
                -> Maybe From
                -> Maybe Page
                -> Maybe To
                -> APIKey
                -> Lastfm Response
getArtistTracks user artist startTimestamp page endTimestamp apiKey = dispatch $ callAPI "user.getartisttracks"
  [ "user" ?< user
  , "artist" ?< artist
  , "startTimestamp" ?< startTimestamp
  , "page" ?< page
  , "endTimestamp" ?< endTimestamp
  , "api_key" ?< apiKey
  ]

getBannedTracks :: User -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getBannedTracks user limit page apiKey = dispatch $ callAPI "user.getbannedtracks"
  [ "user" ?< user
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getEvents :: User -> Maybe Page -> Maybe Limit -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents user page limit festivalsOnly apiKey = dispatch $ callAPI "user.getevents"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "festivalsonly" ?< festivalsOnly
  , "api_key" ?< apiKey
  ]

getFriends :: User -> Maybe RecentTracks -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getFriends user recentTracks limit page apiKey = dispatch $ callAPI "user.getfriends"
  [ "user" ?< user
  , "recenttracks" ?< recentTracks
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getInfo :: Maybe User -> APIKey -> Lastfm Response
getInfo user apiKey = dispatch $ callAPI "user.getinfo"
  [ "user" ?< user
  , "api_key" ?< apiKey
  ]

getLovedTracks :: User -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getLovedTracks user limit page apiKey = dispatch $ callAPI "user.getlovedtracks"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

