{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Chart API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

#include "chart.docs"

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Chart as API

$(xml ["getHypedArtists", "getHypedTracks", "getLovedTracks", "getTopArtists", "getTopTags", "getTopTracks"])

__getHypedArtists__
getHypedArtists ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getHypedTracks__
getHypedTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getLovedTracks__
getLovedTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopArtists__
getTopArtists ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopTags__
getTopTags ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopTracks__
getTopTracks ∷ Maybe Page → Maybe Limit → APIKey → Lastfm Response
