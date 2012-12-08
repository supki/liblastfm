{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Geo (noauth) where

import Control.Applicative ((<$>),(<*>))
import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Geo
import Test.HUnit

import Common


noauth ∷ [Test]
noauth =
  [ TestLabel "Geo.getEvents" $ TestCase testGetEvents
  , TestLabel "Geo.getMetroArtistChart" $ TestCase testGetMetroArtistChart
  , TestLabel "Geo.getMetroHypeArtistChart" $ TestCase testGetMetroHypeArtistChart
  , TestLabel "Geo.getMetroHypeTrackChart" $ TestCase testGetMetroHypeTrackChart
  , TestLabel "Geo.getMetroTrackChart" $ TestCase testGetMetroTrackChart
  , TestLabel "Geo.getMetroUniqueArtistChart" $ TestCase testGetMetroUniqueArtistChart
  , TestLabel "Geo.getMetroUniqueTrackChart" $ TestCase testGetMetroUniqueTrackChart
  , TestLabel "Geo.getMetroWeeklyChartlist" $ TestCase testGetMetroWeeklyChartlist
  , TestLabel "Geo.getMetros" $ TestCase testGetMetros
  , TestLabel "Geo.getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "Geo.getTopTracks" $ TestCase testGetTopTracks
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
