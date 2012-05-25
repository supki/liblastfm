-- | Chart API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

import Network.Lastfm

-- | Get the hyped artists chart.
--
-- More: <http://www.last.fm/api/show/chart.getHypedArtists>
getHypedArtists ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
getHypedArtists = get "getHypedArtists"

-- | Get the hyped tracks chart.
--
-- More: <http://www.last.fm/api/show/chart.getHypedTracks>
getHypedTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
getHypedTracks = get "getHypedTracks"

-- | Get the most loved tracks chart.
--
-- More: <http://www.last.fm/api/show/chart.getLovedTracks>
getLovedTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
getLovedTracks = get "getLovedTracks"

-- | Get the top artists chart.
--
-- More: <http://www.last.fm/api/show/chart.getTopArtists>
getTopArtists ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists = get "getTopArtists"

-- | Get top tags chart.
--
-- More: <http://www.last.fm/api/show/chart.getTopTags>
getTopTags ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTags = get "getTopTags"

-- | Get the top tracks chart.
--
-- More: <http://www.last.fm/api/show/chart.getTopTracks>
getTopTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks = get "getTopTracks"

get ∷ String → Maybe Page → Maybe Limit → APIKey → Lastfm Response
get method page limit apiKey = callAPI XML
  [ (#) (Method $ "chart." ++ method)
  , (#) page
  , (#) limit
  , (#) apiKey
  ]
