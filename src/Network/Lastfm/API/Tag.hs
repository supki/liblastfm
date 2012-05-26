module Network.Lastfm.API.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

import Network.Lastfm

getInfo ∷ ResponseType → Tag → Maybe Language → APIKey → Lastfm Response
getInfo t tag language apiKey = callAPI t
  [ (#) (Method "tag.getInfo")
  , (#) tag
  , (#) language
  , (#) apiKey
  ]

getSimilar ∷ ResponseType → Tag → APIKey → Lastfm Response
getSimilar t tag apiKey = callAPI t
  [ (#) (Method "tag.getSimilar")
  , (#) tag
  , (#) apiKey
  ]

getTopAlbums ∷ ResponseType → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopAlbums t tag page limit apiKey = callAPI t
  [ (#) (Method "tag.getTopAlbums")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopArtists ∷ ResponseType → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists t tag limit page apiKey = callAPI t
  [ (#) (Method "tag.getTopArtists")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopTags ∷ ResponseType → APIKey → Lastfm Response
getTopTags t apiKey = callAPI t
  [ (#) (Method "tag.getTopArtists")
  , (#) apiKey
  ]

getTopTracks ∷ ResponseType → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks t tag limit page apiKey = callAPI t
  [ (#) (Method "tag.getTopTracks")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getWeeklyArtistChart ∷ ResponseType → Tag → Maybe From → Maybe To → Maybe Limit → APIKey → Lastfm Response
getWeeklyArtistChart t tag from to limit apiKey = callAPI t
  [ (#) (Method "tag.getWeeklyArtistChart")
  , (#) tag
  , (#) from
  , (#) to
  , (#) limit
  , (#) apiKey
  ]

getWeeklyChartList ∷ ResponseType → Tag → APIKey → Lastfm Response
getWeeklyChartList t tag apiKey = callAPI t
  [ (#) (Method "tag.getWeeklyChartList")
  , (#) tag
  , (#) apiKey
  ]

search ∷ ResponseType → Tag → Maybe Page → Maybe Limit → APIKey → Lastfm Response
search t tag page limit apiKey = callAPI t
  [ (#) (Method "tag.search")
  , (#) tag
  , (#) page
  , (#) limit
  , (#) apiKey
  ]
