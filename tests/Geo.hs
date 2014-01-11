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


noauth :: Request JSON APIKey -> [Test]
noauth ak =
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
    getEvents <* location "Moscow" <* limit 5 <*> ak

  testGetMetroArtistChart = query ga $
    getMetroArtistChart <*> metro "Saint Petersburg" <*> country "Russia" <*> ak

  testGetMetroHypeArtistChart = query ga $
    getMetroHypeArtistChart <*> metro "New York" <*> country "United States" <*> ak

  testGetMetroHypeTrackChart = query gt $
    getMetroHypeTrackChart <*> metro "Moscow" <*> country "Russia" <*> ak

  testGetMetroTrackChart = query gt $
    getMetroTrackChart <*> metro "Boston" <*> country "United States" <*> ak

  testGetMetroUniqueArtistChart = query ga $
    getMetroUniqueArtistChart <*> metro "Minsk" <*> country "Belarus" <*> ak

  testGetMetroUniqueTrackChart = query gt $
    getMetroUniqueTrackChart <*> metro "Moscow" <*> country "Russia" <*> ak

  testGetMetroWeeklyChartlist = query gc $
    getMetroWeeklyChartlist <*> metro "Moscow" <*> ak

  testGetMetros = query gm $
    getMetros <* country "Russia" <*> ak

  testGetTopArtists = query ga $
    getTopArtists <*> country "Belarus" <* limit 3 <*> ak

  testGetTopTracks = query gt $
    getTopTracks <*> country "Ukraine" <* limit 2 <*> ak


ga, gc, ge, gm, gt :: Query Text
ga = key "topartists".key "artist".values.key "name"._String
gc = key "weeklychartlist".key "chart".values.key "from"._String
ge = key "events".key "event".values.key "id"._String
gm = key "metros".key "metro".values.key "name"._String
gt = key "toptracks".key "track".values.key "name"._String
