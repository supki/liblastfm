{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Tag (noauth) where

import Control.Applicative ((<$>),(<*>))
import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Tag
import Test.HUnit

import Common


noauth ∷ [Test]
noauth =
  [ TestLabel "Tag.getInfo" $ TestCase testGetInfo
  , TestLabel "Tag.getSimilar" $ TestCase testGetSimilar
  , TestLabel "Tag.getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "Tag.getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "Tag.getTopTags" $ TestCase testGetTopTags
  , TestLabel "Tag.getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "Tag.getWeeklyArtistChart" $ TestCase testGetWeeklyArtistChart
  , TestLabel "Tag.getWeeklyChartList" $ TestCase testGetWeeklyChartList
  , TestLabel "Tag.search" $ TestCase testSearch
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetInfo = check gi $
    getInfo "depressive" <> apiKey ak

  testGetSimilar = check gs $
    getSimilar "depressive" <> apiKey ak

  testGetTopAlbums = check gta $
    getTopAlbums "depressive" <> limit 2 <> apiKey ak

  testGetTopArtists = check gtar $
    getTopArtists "depressive" <> limit 3 <> apiKey ak

  testGetTopTags = check gtt $
    getTopTags <> apiKey ak

  testGetTopTracks = check gttr $
    getTopTracks "depressive" <> limit 2 <> apiKey ak

  testGetWeeklyArtistChart = check gwac $
    getWeeklyArtistChart "depressive" <> limit 3 <> apiKey ak

  testGetWeeklyChartList = check gc $
    getWeeklyChartList "depressive" <> apiKey ak

  testSearch = check se $
    search "depressive" <> limit 3 <> apiKey ak


gi ∷ Value → Parser String
gs, gta, gtar, gtt, gttr, gwac, se ∷ Value → Parser [String]
gc ∷ Value → Parser [(String, String)]
gi o = parseJSON o >>= (.: "tag") >>= (.: "taggings")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to"))
gs o = parseJSON o >>= (.: "similartags") >>= (.: "tag") >>= mapM (.: "name")
gta o = parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "url")
gtar o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "url")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gttr o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "url")
gwac o = parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "name")
se o = parseJSON o >>= (.: "results") >>= (.: "tagmatches") >>= (.: "tag") >>= mapM (.: "name")
