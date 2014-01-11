{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Chart (noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Chart
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


noauth :: Request JSON APIKey -> [Test]
noauth ak =
  [ testCase "Chart.getHypedArtists" testGetHypedArtists
  , testCase "Chart.getHypedTracks" testGetHypedTracks
  , testCase "Chart.getLovedTracks" testGetLovedTracks
  , testCase "Chart.getTopArtists" testGetTopArtists
  , testCase "Chart.getTopTags" testGetTopTags
  , testCase "Chart.getTopTracks" testGetTopTracks
  ]
 where
  testGetHypedArtists = query ga  (getHypedArtists <* limit 3 <*> ak)
  testGetHypedTracks  = query gt  (getHypedTracks <* limit 2 <*> ak)
  testGetLovedTracks  = query gt  (getLovedTracks <* limit 3 <*> ak)
  testGetTopArtists   = query ga  (getTopArtists <* limit 4 <*> ak)
  testGetTopTags      = query gta (getTopTags <* limit 5 <*> ak)
  testGetTopTracks    = query gt  (getTopTracks <* limit 2 <*> ak)


ga, gt, gta :: Query Text
ga  = key "artists".key "artist".values.key "name"._String
gt  = key "tracks".key "track".values.key "name"._String
gta = key "tags".key "tag".values.key "name"._String
