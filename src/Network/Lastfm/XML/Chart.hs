{-# LANGUAGE TemplateHaskell #-}
-- | Chart API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Chart as API

$(xml ["getHypedArtists", "getHypedTracks", "getLovedTracks", "getTopArtists", "getTopTags", "getTopTracks"])

-- | Get the hyped artists chart.
--
-- More: <http://www.last.fm/api/show/chart.getHypedArtists>
getHypedArtists ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the hyped tracks chart.
--
-- More: <http://www.last.fm/api/show/chart.getHypedTracks>
getHypedTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the most loved tracks chart.
--
-- More: <http://www.last.fm/api/show/chart.getLovedTracks>
getLovedTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the top artists chart.
--
-- More: <http://www.last.fm/api/show/chart.getTopArtists>
getTopArtists ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get top tags chart.
--
-- More: <http://www.last.fm/api/show/chart.getTopTags>
getTopTags ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the top tracks chart.
--
-- More: <http://www.last.fm/api/show/chart.getTopTracks>
getTopTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
