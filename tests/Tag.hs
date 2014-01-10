{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Tag (noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Tag
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


noauth :: Request JSON APIKey -> [Test]
noauth ak =
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
  testGetInfo = check gi $
    getInfo <*> tag "depressive" <*> ak

  testGetSimilar = check gs $
    getSimilar <*> tag "depressive" <*> ak

  testGetTopAlbums = check gta $
    getTopAlbums <*> tag "depressive" <* limit 2 <*> ak

  testGetTopArtists = check gtar $
    getTopArtists <*> tag "depressive" <* limit 3 <*> ak

  testGetTopTags = check gtt $
    getTopTags <*> ak

  testGetTopTracks = check gttr $
    getTopTracks <*> tag "depressive" <* limit 2 <*> ak

  testGetWeeklyArtistChart = check gwac $
    getWeeklyArtistChart <*> tag "depressive" <* limit 3 <*> ak

  testGetWeeklyChartList = check gc $
    getWeeklyChartList <*> tag "depressive" <*> ak

  testSearch = check se $
    search <*> tag "depressive" <* limit 3 <*> ak


gi :: Value -> Parser String
gs, gta, gtar, gtt, gttr, gwac, se :: Value -> Parser [String]
gc :: Value -> Parser [(String, String)]
gi o = parseJSON o >>= (.: "tag") >>= (.: "taggings")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t -> (,) <$> (t .: "from") <*> (t .: "to"))
gs o = parseJSON o >>= (.: "similartags") >>= (.: "tag") >>= mapM (.: "name")
gta o = parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "url")
gtar o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "url")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gttr o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "url")
gwac o = parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "name")
se o = parseJSON o >>= (.: "results") >>= (.: "tagmatches") >>= (.: "tag") >>= mapM (.: "name")
