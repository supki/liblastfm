module Network.Lastfm.API.User
  ( getArtistTracks, getBannedTracks, getEvents, getFriends, getInfo, getLovedTracks
  , getNeighbours, getNewReleases, getPastEvents, getPersonalTags, getPlaylists, getRecentStations
  , getRecentTracks, getRecommendedArtists, getRecommendedEvents, getShouts, getTopAlbums
  , getTopArtists, getTopTags, getTopTracks, getWeeklyAlbumChart, getWeeklyArtistChart
  , getWeeklyChartList, getWeeklyTrackChart, shout
  ) where

import Network.Lastfm.Internal

getArtistTracks ∷ ResponseType → User → Artist → Maybe StartTimestamp → Maybe EndTimestamp → Maybe Page → APIKey → Lastfm Response
getArtistTracks t user artist startTimestamp endTimestamp page apiKey = callAPI t
  [ (#) (Method "user.getArtistTracks")
  , (#) user
  , (#) artist
  , (#) startTimestamp
  , (#) page
  , (#) endTimestamp
  , (#) apiKey
  ]

getBannedTracks ∷ ResponseType → User → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getBannedTracks t user page limit apiKey = callAPI t
  [ (#) (Method "user.getBannedTracks")
  , (#) user
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getEvents ∷ ResponseType → User → Maybe Page → Maybe Limit → Maybe FestivalsOnly → APIKey → Lastfm Response
getEvents t user page limit festivalsOnly apiKey = callAPI t
  [ (#) (Method "user.getEvents")
  , (#) user
  , (#) page
  , (#) limit
  , (#) festivalsOnly
  , (#) apiKey
  ]

getFriends ∷ ResponseType → User → Maybe RecentTracks → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getFriends t user recentTracks page limit apiKey = callAPI t
  [ (#) (Method "user.getFriends")
  , (#) user
  , (#) recentTracks
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getInfo ∷ ResponseType → Maybe User → APIKey → Lastfm Response
getInfo t user apiKey = callAPI t
  [ (#) (Method "user.getInfo")
  , (#) user
  , (#) apiKey
  ]

getLovedTracks ∷ ResponseType → User → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getLovedTracks t user page limit apiKey = callAPI t
  [ (#) (Method "user.getLovedTracks")
  , (#) user
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getNeighbours ∷ ResponseType → User → Maybe Limit → APIKey → Lastfm Response
getNeighbours t user limit apiKey = callAPI t
  [ (#) (Method "user.getNeighbours")
  , (#) user
  , (#) limit
  , (#) apiKey
  ]

getNewReleases ∷ ResponseType → User → Maybe UseRecs → APIKey → Lastfm Response
getNewReleases t user useRecs apiKey = callAPI t
  [ (#) (Method "user.getNewReleases")
  , (#) user
  , (#) useRecs
  , (#) apiKey
  ]

getPastEvents ∷ ResponseType → User → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getPastEvents t user page limit apiKey = callAPI t
  [ (#) (Method "user.getPastEvents")
  , (#) user
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getPersonalTags ∷ ResponseType
                → User
                → Tag
                → TaggingType
                → Maybe Page
                → Maybe Limit
                → APIKey
                → Lastfm Response
getPersonalTags t user tag taggingType page limit apiKey = callAPI t
  [ (#) (Method "user.getPersonalTags")
  , (#) user
  , (#) tag
  , (#) taggingType
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getPlaylists ∷ ResponseType → User → APIKey → Lastfm Response
getPlaylists t user apiKey = callAPI t
  [ (#) (Method "user.getPlaylists")
  , (#) user
  , (#) apiKey
  ]

getRecentStations ∷ ResponseType → User → Maybe Page → Maybe Limit → APIKey → SessionKey → Secret → Lastfm Response
getRecentStations t user page limit apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "user.getRecentStations")
  , (#) user
  , (#) page
  , (#) limit
  , (#) apiKey
  , (#) sessionKey
  ]

getRecentTracks ∷ ResponseType → User → Maybe Page → Maybe Limit → Maybe From → Maybe To → APIKey → Lastfm Response
getRecentTracks t user page limit from to apiKey = callAPI t
  [ (#) (Method "user.getRecentTracks")
  , (#) user
  , (#) page
  , (#) limit
  , (#) from
  , (#) to
  , (#) apiKey
  ]

getRecommendedArtists ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → SessionKey → Secret → Lastfm Response
getRecommendedArtists t page limit apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "user.getRecommendedArtists")
  , (#) page
  , (#) limit
  , (#) apiKey
  , (#) sessionKey
  ]

getRecommendedEvents ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → SessionKey → Secret → Lastfm Response
getRecommendedEvents t page limit apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "user.getRecommendedEvents")
  , (#) page
  , (#) limit
  , (#) apiKey
  , (#) sessionKey
  ]

getShouts ∷ ResponseType → User → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getShouts t user page limit apiKey = callAPI t
  [ (#) (Method "user.getShouts")
  , (#) user
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopAlbums ∷ ResponseType → User → Maybe Period → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopAlbums t user period page limit apiKey = callAPI t
  [ (#) (Method "user.getTopAlbums")
  , (#) user
  , (#) period
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopArtists ∷ ResponseType → User → Maybe Period → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists t user period page limit apiKey = callAPI t
  [ (#) (Method "user.getTopArtists")
  , (#) user
  , (#) period
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopTags ∷ ResponseType → User → Maybe Limit → APIKey → Lastfm Response
getTopTags t user limit apiKey = callAPI t
  [ (#) (Method "user.getTopTags")
  , (#) user
  , (#) limit
  , (#) apiKey
  ]

getTopTracks ∷ ResponseType → User → Maybe Period → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks t user period page limit apiKey = callAPI t
  [ (#) (Method "user.getTopTracks")
  , (#) user
  , (#) period
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getWeeklyAlbumChart ∷ ResponseType → User → Maybe From → Maybe To → APIKey → Lastfm Response
getWeeklyAlbumChart t user from to apiKey = callAPI t
  [ (#) (Method "user.getWeeklyAlbumChart")
  , (#) user
  , (#) from
  , (#) to
  , (#) apiKey
  ]

getWeeklyArtistChart ∷ ResponseType → User → Maybe From → Maybe To → APIKey → Lastfm Response
getWeeklyArtistChart t user from to apiKey = callAPI t
  [ (#) (Method "user.getWeeklyArtistChart")
  , (#) user
  , (#) from
  , (#) to
  , (#) apiKey
  ]

getWeeklyChartList ∷ ResponseType → User → APIKey → Lastfm Response
getWeeklyChartList t user apiKey = callAPI t
  [ (#) (Method "user.getWeeklyChartList")
  , (#) user
  , (#) apiKey
  ]

getWeeklyTrackChart ∷ ResponseType → User → Maybe From → Maybe To → APIKey → Lastfm Response
getWeeklyTrackChart t user from to apiKey = callAPI t
  [ (#) (Method "user.getWeeklyTrackChart")
  , (#) user
  , (#) from
  , (#) to
  , (#) apiKey
  ]

shout ∷ ResponseType → User → Message → APIKey → SessionKey → Secret → Lastfm Response
shout t user message apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "user.shout")
  , (#) user
  , (#) message
  , (#) apiKey
  , (#) sessionKey
  ]
