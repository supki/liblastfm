{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Geo (noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Geo
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


noauth ∷ Request JSON Send APIKey → [Test]
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
  testGetEvents = check ge $
    getEvents <* location "Moscow" <* limit 5 <*> ak

  testGetMetroArtistChart = check ga $
    getMetroArtistChart <*> metro "Saint Petersburg" <*> country "Russia" <*> ak

  testGetMetroHypeArtistChart = check ga $
    getMetroHypeArtistChart <*> metro "New York" <*> country "United States" <*> ak

  testGetMetroHypeTrackChart = check gt $
    getMetroHypeTrackChart <*> metro "Ufa" <*> country "Russia" <*> ak

  testGetMetroTrackChart = check gt $
    getMetroTrackChart <*> metro "Boston" <*> country "United States" <*> ak

  testGetMetroUniqueArtistChart = check ga $
    getMetroUniqueArtistChart <*> metro "Minsk" <*> country "Belarus" <*> ak

  testGetMetroUniqueTrackChart = check gt $
    getMetroUniqueTrackChart <*> metro "Moscow" <*> country "Russia" <*> ak

  testGetMetroWeeklyChartlist = check gc $
    getMetroWeeklyChartlist <*> metro "Moscow" <*> ak

  testGetMetros = check gm $
    getMetros <* country "Russia" <*> ak

  testGetTopArtists = check ga $
    getTopArtists <*> country "Belarus" <* limit 3 <*> ak

  testGetTopTracks = check gt $
    getTopTracks <*> country "Ukraine" <* limit 2 <*> ak


ga, ge, gm, gt ∷ Value → Parser [String]
gc ∷ Value → Parser [(String, String)]
ga o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to"))
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "id")
gm o = parseJSON o >>= (.: "metros") >>= (.: "metro") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name")
