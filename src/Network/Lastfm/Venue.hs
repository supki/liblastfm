{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm venue API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Venue as Venue
-- @
module Network.Lastfm.Venue
  ( getEvents, getPastEvents, search
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Get a list of upcoming events at this venue.
--
-- Optional: 'festivalsonly'
--
-- <http://www.last.fm/api/show/venue.getEvents>
getEvents ∷ Venue → Request f Ready t
getEvents v = api "venue.getEvents" <> venue v


-- | Get a paginated list of all the events held at this venue in the past.
--
-- Optional: 'festivalsonly', 'page', 'limit'
--
-- <http://www.last.fm/api/show/venue.getPastEvents>
getPastEvents ∷ Venue → Request f Ready t
getPastEvents v = api "venue.getPastEvents" <> venue v


-- | Search for a venue by venue name
--
-- Optional: 'page', 'limit', 'country'
--
-- <http://www.last.fm/api/show/venue.search>
search ∷ VenueName → Request f Ready t
search v = api "venue.search" <> venueName v
