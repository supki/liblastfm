{-# LANGUAGE TemplateHaskell #-}
-- | Venue API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Venue
  ( getEvents, getPastEvents, search
  ) where

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Venue as API

$(json ["getEvents", "getPastEvents", "search"])

-- | Get a list of upcoming events at this venue.
--
-- More: <http://www.last.fm/api/show/venue.getEvents>
getEvents ∷ Venue → Maybe FestivalsOnly → APIKey → Lastfm Response

-- | Get a paginated list of all the events held at this venue in the past.
--
-- More: <http://www.last.fm/api/show/venue.getPastEvents>
getPastEvents ∷ Venue → Maybe FestivalsOnly → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Search for a venue by venue name.
--
-- More: <http://www.last.fm/api/show/venue.search>
search ∷ Venuename → Maybe Page → Maybe Limit → Maybe Country → APIKey → Lastfm Response
