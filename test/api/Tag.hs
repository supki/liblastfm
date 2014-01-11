{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Tag (noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Tag
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


noauth :: [Test]
noauth =
  [ testCase "Tag.getInfo" testGetInfo
  , testCase "Tag.getSimilar" testGetSimilar
  , testCase "Tag.getTopAlbums" testGetTopAlbums
  , testCase "Tag.getTopArtists" testGetTopArtists
  , testCase "Tag.getTopTags" testGetTopTags
  , testCase "Tag.getTopTracks" testGetTopTracks
  , testCase "Tag.getWeeklyArtistChart" testGetWeeklyArtistChart
  , testCase "Tag.getWeeklyChartList" testGetWeeklyChartList
  , testCase "Tag.search" testSearch
  ]
 where
  testGetInfo = query gi $
    getInfo <*> tag "depressive" <*> publicKey

  testGetSimilar = query gs $
    getSimilar <*> tag "depressive" <*> publicKey

  testGetTopAlbums = query gta $
    getTopAlbums <*> tag "depressive" <* limit 2 <*> publicKey

  testGetTopArtists = query gtar $
    getTopArtists <*> tag "depressive" <* limit 3 <*> publicKey

  testGetTopTags = query gtt $
    getTopTags <*> publicKey

  testGetTopTracks = query gttr $
    getTopTracks <*> tag "depressive" <* limit 2 <*> publicKey

  testGetWeeklyArtistChart = query gwac $
    getWeeklyArtistChart <*> tag "depressive" <* limit 3 <*> publicKey

  testGetWeeklyChartList = query gc $
    getWeeklyChartList <*> tag "depressive" <*> publicKey

  testSearch = query se $
    search <*> tag "depressive" <* limit 3 <*> publicKey


gc, gi, gs, gta, gtar, gtt, gttr, gwac, se :: Query Text
gi   = key "tag".key "taggings"._String
gc   = key "weeklychartlist".key "chart".values.key "from"._String
gs   = key "similartags".key "tag".values.key "name"._String
gta  = key "topalbums".key "album".values.key "url"._String
gtar = key "topartists".key "artist".values.key "url"._String
gtt  = key "toptags".key "tag".values.key "name"._String
gttr = key "toptracks".key "track".values.key "url"._String
gwac = key "weeklyartistchart".key "artist".values.key "name"._String
se   = key "results".key "tagmatches".key "tag".values.key "name"._String
