{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

  testGetEvents = assert $ parse ge <:> lastfm (
    getEvents <> location "Moscow" <> limit 5 <> apiKey ak <> json)

  testGetMetroArtistChart = assert $ parse ga <:> lastfm (
    getMetroArtistChart "Saint Petersburg" "Russia" <> apiKey ak <> json)

  testGetMetroHypeArtistChart = assert $ parse ga <:> lastfm (
    getMetroHypeArtistChart "New York" "United States" <> apiKey ak <> json)

  testGetMetroHypeTrackChart = assert $ parse gt <:> lastfm (
    getMetroHypeTrackChart "Ufa" "Russia" <> apiKey ak <> json)

  testGetMetroTrackChart = assert $ parse gt <:> lastfm (
    getMetroTrackChart "Boston" "United States" <> apiKey ak <> json)

  testGetMetroUniqueArtistChart = assert $ parse ga <:> lastfm (
    getMetroUniqueArtistChart "Minsk" "Belarus" <> apiKey ak <> json)

  testGetMetroUniqueTrackChart = assert $ parse gt <:> lastfm (
    getMetroUniqueTrackChart "Moscow" "Russia" <> apiKey ak <> json)

  testGetMetroWeeklyChartlist = assert $ parse gc <:> lastfm (
    getMetroWeeklyChartlist "Moscow" <> apiKey ak <> json)

  testGetMetros = assert $ parse gm <:> lastfm (
    getMetros <> country "Russia" <> apiKey ak <> json)

  testGetTopArtists = assert $ parse ga <:> lastfm (
    getTopArtists "Belarus" <> limit 3 <> apiKey ak <> json)

  testGetTopTracks = assert $ parse gt <:> lastfm (
    getTopTracks "Ukraine" <> limit 2 <> apiKey ak <> json)


ga, ge, gm, gt ∷ Value → Parser [String]
gc ∷ Value → Parser [(String, String)]
ga o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to"))
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "id")
gm o = parseJSON o >>= (.: "metros") >>= (.: "metro") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name")
