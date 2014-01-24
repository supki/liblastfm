{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TagSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Tag
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Tag.getInfo" $
    query gi $
      getInfo <*> tag "depressive" <*> publicKey

  it "Tag.getSimilar" $
    query gs $
      getSimilar <*> tag "depressive" <*> publicKey

  it "Tag.getTopAlbums" $
    query gta $
      getTopAlbums <*> tag "depressive" <* limit 2 <*> publicKey

  it "Tag.getTopArtists" $
    query gtar $
      getTopArtists <*> tag "depressive" <* limit 3 <*> publicKey

  it "Tag.getTopTags" $
    query gtt $
      getTopTags <*> publicKey

  it "Tag.getTopTracks" $
    query gttr $
      getTopTracks <*> tag "depressive" <* limit 2 <*> publicKey

  it "Tag.getWeeklyArtistChart" $
    query gwac $
      getWeeklyArtistChart <*> tag "depressive" <* limit 3 <*> publicKey

  it "Tag.getWeeklyChartList" $
    query gc $
      getWeeklyChartList <*> tag "depressive" <*> publicKey

  it "Tag.search" $
    query se $
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
