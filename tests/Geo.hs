{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Geo (noauth) where

import Control.Applicative ((<$>),(<*>))
import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Geo
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


noauth ∷ [Test]
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
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetEvents = check ge $
    getEvents <> location "Moscow" <> limit 5 <> apiKey ak

  testGetMetroArtistChart = check ga $
    getMetroArtistChart "Saint Petersburg" "Russia" <> apiKey ak

  testGetMetroHypeArtistChart = check ga $
    getMetroHypeArtistChart "New York" "United States" <> apiKey ak

  testGetMetroHypeTrackChart = check gt $
    getMetroHypeTrackChart "Ufa" "Russia" <> apiKey ak

  testGetMetroTrackChart = check gt $
    getMetroTrackChart "Boston" "United States" <> apiKey ak

  testGetMetroUniqueArtistChart = check ga $
    getMetroUniqueArtistChart "Minsk" "Belarus" <> apiKey ak

  testGetMetroUniqueTrackChart = check gt $
    getMetroUniqueTrackChart "Moscow" "Russia" <> apiKey ak

  testGetMetroWeeklyChartlist = check gc $
    getMetroWeeklyChartlist "Moscow" <> apiKey ak

  testGetMetros = check gm $
    getMetros <> country "Russia" <> apiKey ak

  testGetTopArtists = check ga $
    getTopArtists "Belarus" <> limit 3 <> apiKey ak

  testGetTopTracks = check gt $
    getTopTracks "Ukraine" <> limit 2 <> apiKey ak


ga, ge, gm, gt ∷ Value → Parser [String]
gc ∷ Value → Parser [(String, String)]
ga o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to"))
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "id")
gm o = parseJSON o >>= (.: "metros") >>= (.: "metro") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name")
