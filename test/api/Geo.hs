{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Geo (noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Geo
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


noauth :: [Test]
noauth =
  [ testCase "Geo.getEvents" testGetEvents
  , testCase "Geo.getMetroArtistChart" testGetMetroArtistChart
  , testCase "Geo.getMetroHypeArtistChart" testGetMetroHypeArtistChart
  , testCase "Geo.getMetroHypeTrackChart" testGetMetroHypeTrackChart
  , testCase "Geo.getMetroTrackChart" testGetMetroTrackChart
  , testCase "Geo.getMetroUniqueArtistChart" testGetMetroUniqueArtistChart
  , testCase "Geo.getMetroUniqueTrackChart" testGetMetroUniqueTrackChart
  , testCase "Geo.getMetroWeeklyChartlist" testGetMetroWeeklyChartlist
  , testCase "Geo.getMetros" testGetMetros
  , testCase "Geo.getTopArtists" testGetTopArtists
  , testCase "Geo.getTopTracks" testGetTopTracks
  ]
 where
  testGetEvents = query ge $
    getEvents <* location "Moscow" <* limit 5 <*> publicKey

  testGetMetroArtistChart = query ga $
    getMetroArtistChart <*> metro "Saint Petersburg" <*> country "Russia" <*> publicKey

  testGetMetroHypeArtistChart = query ga $
    getMetroHypeArtistChart <*> metro "New York" <*> country "United States" <*> publicKey

  testGetMetroHypeTrackChart = query gt $
    getMetroHypeTrackChart <*> metro "Moscow" <*> country "Russia" <*> publicKey

  testGetMetroTrackChart = query gt $
    getMetroTrackChart <*> metro "Boston" <*> country "United States" <*> publicKey

  testGetMetroUniqueArtistChart = query ga $
    getMetroUniqueArtistChart <*> metro "Minsk" <*> country "Belarus" <*> publicKey

  testGetMetroUniqueTrackChart = query gt $
    getMetroUniqueTrackChart <*> metro "Moscow" <*> country "Russia" <*> publicKey

  testGetMetroWeeklyChartlist = query gc $
    getMetroWeeklyChartlist <*> metro "Moscow" <*> publicKey

  testGetMetros = query gm $
    getMetros <* country "Russia" <*> publicKey

  testGetTopArtists = query ga $
    getTopArtists <*> country "Belarus" <* limit 3 <*> publicKey

  testGetTopTracks = query gt $
    getTopTracks <*> country "Ukraine" <* limit 2 <*> publicKey


ga, gc, ge, gm, gt :: Query Text
ga = key "topartists".key "artist".values.key "name"._String
gc = key "weeklychartlist".key "chart".values.key "from"._String
ge = key "events".key "event".values.key "id"._String
gm = key "metros".key "metro".values.key "name"._String
gt = key "toptracks".key "track".values.key "name"._String
