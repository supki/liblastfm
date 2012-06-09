module Network.Lastfm.API.Venue
  ( getEvents, getPastEvents, search
  ) where

import Network.Lastfm.Internal

getEvents ∷ ResponseType → Venue → Maybe FestivalsOnly → APIKey → Lastfm Response
getEvents t venue festivalsOnly apiKey = callAPI t
  [ (#) (Method "venue.getEvents")
  , (#) venue
  , (#) festivalsOnly
  , (#) apiKey
  ]

getPastEvents ∷ ResponseType → Venue → Maybe FestivalsOnly → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getPastEvents t venue festivalsOnly page limit apiKey = callAPI t
  [ (#) (Method "venue.getPastEvents")
  , (#) venue
  , (#) festivalsOnly
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

search ∷ ResponseType → Venuename → Maybe Page → Maybe Limit → Maybe Country → APIKey → Lastfm Response
search t venue page limit country apiKey = callAPI t
  [ (#) (Method "venue.search")
  , (#) venue
  , (#) page
  , (#) limit
  , (#) country
  , (#) apiKey
  ]
