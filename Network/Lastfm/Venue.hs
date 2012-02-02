module Network.Lastfm.Venue
  ( getEvents, getPastEvents, search
  ) where

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), APIKey, Country, FestivalsOnly, Limit, Page, Venue)

getEvents :: Venue -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents venue festivalsOnly apiKey = dispatch $ callAPI "venue.getEvents"
  [ "venue" ?< venue
  , "api_key" ?< apiKey
  , "festivalsonly" ?< festivalsOnly
  ]

getPastEvents :: Venue -> Maybe FestivalsOnly -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getPastEvents venue festivalsOnly page limit apiKey = dispatch $ callAPI "venue.getPastEvents"
  [ "venue" ?< venue
  , "api_key" ?< apiKey
  , "festivalsonly" ?< festivalsOnly
  , "page" ?< page
  , "limit" ?< limit
  ]

search :: Venue -> Maybe Page -> Maybe Limit -> Maybe Country -> APIKey -> Lastfm Response
search venue page limit country apiKey = dispatch $ callAPI "venue.search"
  [ "venue" ?< venue
  , "api_key" ?< apiKey
  , "page" ?< page
  , "limit" ?< limit
  , "country" ?< country
  ]
