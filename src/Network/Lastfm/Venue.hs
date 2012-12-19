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

import Network.Lastfm.Request


-- | Get a list of upcoming events at this venue.
--
-- Optional: 'festivalsonly'
--
-- <http://www.last.fm/api/show/venue.getEvents>
getEvents ∷ Request f Send (Venue → APIKey → Ready)
getEvents = api "venue.getEvents"


-- | Get a paginated list of all the events held at this venue in the past.
--
-- Optional: 'festivalsonly', 'page', 'limit'
--
-- <http://www.last.fm/api/show/venue.getPastEvents>
getPastEvents ∷ Request f Send (Venue → APIKey → Ready)
getPastEvents = api "venue.getPastEvents"


-- | Search for a venue by venue name
--
-- Optional: 'page', 'limit', 'country'
--
-- <http://www.last.fm/api/show/venue.search>
search ∷ Request f Send (VenueName → APIKey → Ready)
search = api "venue.search"
