{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.User
  ( Artist(..), EndTimestamp(..), FestivalsOnly(..), Limit(..), Page(..), RecentTracks(..), StartTimestamp(..), User(..)
  , getArtistTracks, getBannedTracks, getEvents, getFriends, getInfo, getLovedTracks
  )where

import Network.Lastfm.Artist (Artist(..))
import Network.Lastfm.Auth (APIKey, SessionKey)
import Network.Lastfm.Core

newtype EndTimestamp = EndTimestamp Int deriving (Show, LastfmValue)
newtype FestivalsOnly = FestivalsOnly Bool deriving (Show, LastfmValue)
newtype Limit = Limit Int deriving (Show, LastfmValue)
newtype Page = Page Int deriving (Show, LastfmValue)
newtype RecentTracks = RecentTracks Bool deriving (Show, LastfmValue)
newtype StartTimestamp = StartTimestamp Int deriving (Show, LastfmValue)
newtype User = User String deriving (Show, LastfmValue)

getArtistTracks :: User
                -> Artist
                -> Maybe StartTimestamp
                -> Maybe Page
                -> Maybe EndTimestamp
                -> APIKey
                -> Lastfm Response
getArtistTracks user artist startTimestamp page endTimestamp apiKey = callAPI "user.getartisttracks"
  [ "user" ?< user
  , "artist" ?< artist
  , "startTimestamp" ?< startTimestamp
  , "page" ?< page
  , "endTimestamp" ?< endTimestamp
  , "api_key" ?< apiKey
  ]

getBannedTracks :: User -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getBannedTracks user limit page apiKey = callAPI "user.getbannedtracks"
  [ "user" ?< user
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getEvents :: User -> Maybe Page -> Maybe Limit -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents user page limit festivalsOnly apiKey = callAPI "user.getevents"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "festivalsonly" ?< festivalsOnly
  , "api_key" ?< apiKey
  ]

getFriends :: User -> Maybe RecentTracks -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getFriends user recentTracks limit page apiKey = callAPI "user.getfriends"
  [ "user" ?< user
  , "recenttracks" ?< recentTracks
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getInfo :: Maybe User -> APIKey -> Lastfm Response
getInfo user apiKey = callAPI "user.getinfo"
  [ "user" ?< user
  , "api_key" ?< apiKey
  ]

getLovedTracks :: User -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getLovedTracks user limit page apiKey = callAPI "user.getlovedtracks"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

