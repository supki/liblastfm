{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Chart (noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Chart
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


noauth ∷ Request JSON APIKey → [Test]
noauth ak =
  [ testCase "Chart.getHypedArtists" testGetHypedArtists
  , testCase "Chart.getHypedTracks" testGetHypedTracks
  , testCase "Chart.getLovedTracks" testGetLovedTracks
  , testCase "Chart.getTopArtists" testGetTopArtists
  , testCase "Chart.getTopTags" testGetTopTags
  , testCase "Chart.getTopTracks" testGetTopTracks
  ]
 where
  testGetHypedArtists = check ga $
    getHypedArtists <* limit 3 <*> ak

  testGetHypedTracks = check gt $
    getHypedTracks <* limit 2 <*> ak

  testGetLovedTracks = check gt $
    getLovedTracks <* limit 3 <*> ak

  testGetTopArtists = check ga $
    getTopArtists <* limit 4 <*> ak

  testGetTopTags = check gta $
    getTopTags <* limit 5 <*> ak

  testGetTopTracks = check gt $
    getTopTracks <* limit 2 <*> ak


ga, gt, gta ∷ Value → Parser [String]
ga o = parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
gta o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
