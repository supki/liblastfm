module Network.Lastfm.API.Group
  ( getHype, getMembers, getWeeklyChartList, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyTrackChart
  ) where

import Network.Lastfm.Internal

getHype ∷ Format → Group → APIKey → Lastfm Response
getHype t group apiKey = callAPI t
  [ (#) (Method "group.getHype")
  , (#) group
  , (#) apiKey
  ]

getMembers ∷ Format → Group → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getMembers t group page limit apiKey = callAPI t
  [ (#) (Method "group.getMembers")
  , (#) group
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getWeeklyChartList ∷ Format → Group → APIKey → Lastfm Response
getWeeklyChartList t group apiKey = callAPI t
  [ (#) (Method "group.getWeeklyChartList")
  , (#) group
  , (#) apiKey
  ]

getWeeklyAlbumChart ∷ Format → Group → Maybe From → Maybe To → APIKey → Lastfm Response
getWeeklyAlbumChart t group from to apiKey = callAPI t
  [ (#) (Method "group.getWeeklyAlbumChart")
  , (#) group
  , (#) from
  , (#) to
  , (#) apiKey
  ]

getWeeklyArtistChart ∷ Format → Group → Maybe From → Maybe To → APIKey → Lastfm Response
getWeeklyArtistChart t group from to apiKey = callAPI t
  [ (#) (Method "group.getWeeklyArtistChart")
  , (#) group
  , (#) from
  , (#) to
  , (#) apiKey
  ]


getWeeklyTrackChart ∷ Format → Group → Maybe From → Maybe To → APIKey → Lastfm Response
getWeeklyTrackChart t group from to apiKey = callAPI t
  [ (#) (Method "group.getWeeklyTrackChart")
  , (#) group
  , (#) from
  , (#) to
  , (#) apiKey
  ]
