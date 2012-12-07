{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

  testGetInfo = assert $ parse gi <:> lastfm (
    getInfo "depressive" <> apiKey ak <> json)

  testGetSimilar = assert $ parse gs <:> lastfm (
    getSimilar "depressive" <> apiKey ak <> json)

  testGetTopAlbums = assert $ parse gta <:> lastfm (
    getTopAlbums "depressive" <> limit 2 <> apiKey ak <> json)

  testGetTopArtists = assert $ parse gtar <:> lastfm (
    getTopArtists "depressive" <> limit 3 <> apiKey ak <> json)

  testGetTopTags = assert $ parse gtt <:> lastfm (
    getTopTags <> apiKey ak <> json)

  testGetTopTracks = assert $ parse gttr <:> lastfm (
    getTopTracks "depressive" <> limit 2 <> apiKey ak <> json)

  testGetWeeklyArtistChart = assert $ parse gwac <:> lastfm (
    getWeeklyArtistChart "depressive" <> limit 3 <> apiKey ak <> json)

  testGetWeeklyChartList = assert $ parse gc <:> lastfm (
    getWeeklyChartList "depressive" <> apiKey ak <> json)

  testSearch = assert $ parse se <:> lastfm (
    search "depressive" <> limit 3 <> apiKey ak <> json)


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
