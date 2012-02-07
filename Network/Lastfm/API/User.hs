module Network.Lastfm.API.User
  ( getArtistTracks, getBannedTracks, getEvents, getFriends, getInfo, getLovedTracks
  , getNeighbours, getNewReleases, getPastEvents, getPersonalTags, getPlaylists, getRecentStations
  , getRecentTracks, getRecommendedArtists, getRecommendedEvents, getShouts, getTopAlbums
  , getTopArtists, getTopTags, getTopTracks, getWeeklyAlbumChart, getWeeklyArtistChart
  , getWeeklyChartList, getWeeklyTrackChart, shout
  ) where

import Network.Lastfm.Response
import Network.Lastfm.Types ( (?<), APIKey, Artist, FestivalsOnly, From, Limit, Message, Page
                            , Period, RecentTracks, SessionKey, Tag, TaggingType, To, User, UseRecs
                            )

getArtistTracks :: User
                -> Artist
                -> Maybe From
                -> Maybe To
                -> Maybe Page
                -> APIKey
                -> Lastfm Response
getArtistTracks user artist startTimestamp endTimestamp page apiKey = dispatch $ callAPI "user.getArtistTracks"
  [ "user" ?< user
  , "artist" ?< artist
  , "startTimestamp" ?< startTimestamp
  , "page" ?< page
  , "endTimestamp" ?< endTimestamp
  , "api_key" ?< apiKey
  ]

getBannedTracks :: User -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getBannedTracks user page limit apiKey = dispatch $ callAPI "user.getBannedTracks"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getEvents :: User -> Maybe Page -> Maybe Limit -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents user page limit festivalsOnly apiKey = dispatch $ callAPI "user.getEvents"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "festivalsonly" ?< festivalsOnly
  , "api_key" ?< apiKey
  ]

getFriends :: User -> Maybe RecentTracks -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getFriends user recentTracks page limit apiKey = dispatch $ callAPI "user.getFriends"
  [ "user" ?< user
  , "recenttracks" ?< recentTracks
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getInfo :: Maybe User -> APIKey -> Lastfm Response
getInfo user apiKey = dispatch $ callAPI "user.getInfo"
  [ "user" ?< user
  , "api_key" ?< apiKey
  ]

getLovedTracks :: User -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getLovedTracks user page limit apiKey = dispatch $ callAPI "user.getLovedTracks"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getNeighbours :: User -> Maybe Limit -> APIKey -> Lastfm Response
getNeighbours user limit apiKey = dispatch $ callAPI "user.getNeighbours"
  [ "user" ?< user
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getNewReleases :: User -> Maybe UseRecs -> APIKey -> Lastfm Response
getNewReleases user useRecs apiKey = dispatch $ callAPI "user.getNewReleases"
  [ "user" ?< user
  , "userecs" ?< useRecs
  , "api_key" ?< apiKey
  ]

getPastEvents :: User -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getPastEvents user page limit apiKey = dispatch $ callAPI "user.getPastEvents"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getPersonalTags :: User
                -> Tag
                -> TaggingType
                -> Maybe Page
                -> Maybe Limit
                -> APIKey
                -> Lastfm Response
getPersonalTags user tag taggingType page limit apiKey = dispatch $ callAPI "user.getPersonalTags"
  [ "user" ?< user
  , "tag" ?< tag
  , "taggingtype" ?< taggingType
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getPlaylists :: User -> APIKey -> Lastfm Response
getPlaylists user apiKey = dispatch $ callAPI "user.getPlaylists"
  [ "user" ?< user
  , "api_key" ?< apiKey
  ]

getRecentStations :: User -> Maybe Page -> Maybe Limit -> APIKey -> SessionKey -> Lastfm Response
getRecentStations user page limit apiKey sessionKey = dispatch $ callAPI "user.getRecentStations"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getRecentTracks :: User -> Maybe Page -> Maybe Limit -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getRecentTracks user page limit from to apiKey = dispatch $ callAPI "user.getRecentTracks"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "from" ?< from
  , "to" ?< to
  , "api_key" ?< apiKey
  ]

getRecommendedArtists :: Maybe Page -> Maybe Limit -> APIKey -> SessionKey -> Lastfm Response
getRecommendedArtists page limit apiKey sessionKey = dispatch $ callAPI "user.getRecommendedArtists"
  [ "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getRecommendedEvents :: Maybe Page -> Maybe Limit -> APIKey -> SessionKey -> Lastfm Response
getRecommendedEvents page limit apiKey sessionKey = dispatch $ callAPI "user.getRecommendedEvents"
  [ "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getShouts :: User -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts user page limit apiKey = dispatch $ callAPI "user.getShouts"
  [ "user" ?< user
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getTopAlbums :: User -> Maybe Period -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopAlbums user period page limit apiKey = dispatch $ callAPI "user.getTopAlbums"
  [ "user" ?< user
  , "period" ?< period
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getTopArtists :: User -> Maybe Period -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopArtists user period page limit apiKey = dispatch $ callAPI "user.getTopArtists"
  [ "user" ?< user
  , "period" ?< period
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getTopTags :: User -> Maybe Limit -> APIKey -> Lastfm Response
getTopTags user limit apiKey = dispatch $ callAPI "user.getTopTags"
  [ "user" ?< user
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getTopTracks :: User -> Maybe Period -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks user period page limit apiKey = dispatch $ callAPI "user.getTopTracks"
  [ "user" ?< user
  , "period" ?< period
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getWeeklyAlbumChart :: User -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyAlbumChart user from to apiKey = dispatch $ callAPI "user.getWeeklyAlbumChart"
  [ "user" ?< user
  , "from" ?< from
  , "to" ?< to
  , "api_key" ?< apiKey
  ]

getWeeklyArtistChart :: User -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyArtistChart user from to apiKey = dispatch $ callAPI "user.getWeeklyArtistChart"
  [ "user" ?< user
  , "from" ?< from
  , "to" ?< to
  , "api_key" ?< apiKey
  ]

getWeeklyChartList :: User -> APIKey -> Lastfm Response
getWeeklyChartList user apiKey = dispatch $ callAPI "user.getWeeklyChartList" ["user" ?< user, "api_key" ?< apiKey]

getWeeklyTrackChart :: User -> Maybe From -> Maybe To -> APIKey -> Lastfm Response
getWeeklyTrackChart user from to apiKey = dispatch $ callAPI "user.getWeeklyTrackChart"
  [ "user" ?< user
  , "from" ?< from
  , "to" ?< to
  , "api_key" ?< apiKey
  ]

shout :: User -> Message -> APIKey -> SessionKey -> Lastfm ()
shout user message apiKey sessionKey = dispatch $ callAPI_ "user.shout"
  [ "user" ?< user
  , "message" ?< message
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
