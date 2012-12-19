{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm chart API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Chart as Chart
-- @
module Network.Lastfm.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

import Network.Lastfm.Request


-- | Get the hyped artists chart
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/chart.getHypedArtists>
getHypedArtists ∷ Request f Send (APIKey → Ready)
getHypedArtists = api "chart.getHypedArtists"


-- | Get the top artists chart
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/chart.getHypedTracks>
getHypedTracks ∷ Request f Send (APIKey → Ready)
getHypedTracks = api "chart.getHypedTracks"


-- | Get the most loved tracks chart
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/chart.getLovedTracks>
getLovedTracks ∷ Request f Send (APIKey → Ready)
getLovedTracks = api "chart.getLovedTracks"


-- | Get the top artists chart
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/chart.getTopArtists>
getTopArtists ∷ Request f Send (APIKey → Ready)
getTopArtists = api "chart.getTopArtists"


-- | Get the top artists chart
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/chart.getTopTags>
getTopTags ∷ Request f Send (APIKey → Ready)
getTopTags = api "chart.getTopTags"


-- | Get the top tracks chart
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/chart.getTopTracks>
getTopTracks ∷ Request f Send (APIKey → Ready)
getTopTracks = api "chart.getTopTracks"
