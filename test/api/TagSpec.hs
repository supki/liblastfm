{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TagSpec (spec) where

import Control.Lens.Aeson
import Network.Lastfm
import Network.Lastfm.Tag
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "getInfo" $
    publicly (getInfo <*> tag "depressive")
   `shouldHaveJson`
    key "tag".key "taggings"._String

  it "getSimilar" $
    publicly (getSimilar <*> tag "depressive")
   `shouldHaveJson`
    key "similartags".key "tag".values.key "name"._String

  it "getTopAlbums" $
    publicly (getTopAlbums <*> tag "depressive" <* limit 2)
   `shouldHaveJson`
    key "topalbums".key "album".values.key "url"._String

  it "getTopArtists" $
    publicly (getTopArtists <*> tag "depressive" <* limit 3)
   `shouldHaveJson`
    key "topartists".key "artist".values.key "url"._String

  it "getTopTags" $
    publicly getTopTags
   `shouldHaveJson`
    key "toptags".key "tag".values.key "name"._String

  it "getTopTracks" $
    publicly (getTopTracks <*> tag "depressive" <* limit 2)
   `shouldHaveJson`
    key "toptracks".key "track".values.key "url"._String

  it "getWeeklyArtistChart" $
    publicly (getWeeklyArtistChart <*> tag "depressive" <* limit 3)
   `shouldHaveJson`
    key "weeklyartistchart".key "artist".values.key "name"._String

  it "getWeeklyChartList" $
    publicly (getWeeklyChartList <*> tag "depressive")
   `shouldHaveJson`
    key "weeklychartlist".key "chart".values.key "from"._String

  it "search" $
    publicly (search <*> tag "depressive" <* limit 3)
   `shouldHaveJson`
    key "results".key "tagmatches".key "tag".values.key "name"._String
