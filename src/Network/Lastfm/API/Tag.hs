module Network.Lastfm.API.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

import Network.Lastfm.Internal

getInfo ∷ Format → Tag → Maybe Language → APIKey → Lastfm Response
getInfo t tag language apiKey = callAPI t
  [ (#) (Method "tag.getInfo")
  , (#) tag
  , (#) language
  , (#) apiKey
  ]

getSimilar ∷ Format → Tag → APIKey → Lastfm Response
getSimilar t tag apiKey = callAPI t
  [ (#) (Method "tag.getSimilar")
  , (#) tag
  , (#) apiKey
  ]

getTopAlbums ∷ Format → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopAlbums t tag page limit apiKey = callAPI t
  [ (#) (Method "tag.getTopAlbums")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopArtists ∷ Format → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists t tag limit page apiKey = callAPI t
  [ (#) (Method "tag.getTopArtists")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopTags ∷ Format → APIKey → Lastfm Response
getTopTags t apiKey = callAPI t
  [ (#) (Method "tag.getTopTags")
  , (#) apiKey
  ]

getTopTracks ∷ Format → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks t tag limit page apiKey = callAPI t
  [ (#) (Method "tag.getTopTracks")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getWeeklyArtistChart ∷ Format → Tag → Maybe From → Maybe To → Maybe Limit → APIKey → Lastfm Response
getWeeklyArtistChart t tag from to limit apiKey = callAPI t
  [ (#) (Method "tag.getWeeklyArtistChart")
  , (#) tag
  , (#) from
  , (#) to
  , (#) limit
  , (#) apiKey
  ]

getWeeklyChartList ∷ Format → Tag → APIKey → Lastfm Response
getWeeklyChartList t tag apiKey = callAPI t
  [ (#) (Method "tag.getWeeklyChartList")
  , (#) tag
  , (#) apiKey
  ]

search ∷ Format → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
search t tag page limit apiKey = callAPI t
  [ (#) (Method "tag.search")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]
