-- | Venue API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Venue
  ( getEvents, getPastEvents, search
  ) where

import Control.Monad.Error (runErrorT)
import Network.Lastfm

-- | Get a list of upcoming events at this venue.
--
-- More: <http://www.lastfm.ru/api/show/venue.getEvents>
getEvents :: Venue -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents venue festivalsOnly apiKey = runErrorT . callAPI $
  [ (#) (Method "venue.getEvents")
  , (#) venue
  , (#) festivalsOnly
  , (#) apiKey
  ]

-- | Get a paginated list of all the events held at this venue in the past.
--
-- More: <http://www.lastfm.ru/api/show/venue.getPastEvents>
getPastEvents :: Venue -> Maybe FestivalsOnly -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getPastEvents venue festivalsOnly page limit apiKey = runErrorT . callAPI $
  [ (#) (Method "venue.getPastEvents")
  , (#) venue
  , (#) festivalsOnly
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Search for a venue by venue name.
--
-- More: <http://www.lastfm.ru/api/show/venue.search>
search :: Name -> Maybe Page -> Maybe Limit -> Maybe Country -> APIKey -> Lastfm Response
search venue page limit country apiKey = runErrorT . callAPI $
  [ (#) (Method "venue.search")
  , (#) venue
  , (#) page
  , (#) limit
  , (#) country
  , (#) apiKey
  ]
