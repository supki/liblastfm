{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.TagSpec (spec) where

import Network.Lastfm
import Network.Lastfm.Tag
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "getInfo" $
    publicly (getInfo <*> tag "depressive")
   `shouldHaveXml`
    root.node "tag".node "taggings".text

  it "getSimilar" $
    publicly (getSimilar <*> tag "depressive")
   `shouldHaveXml`
    root.node "similartags".node "tag".node "name".text

  it "getTopAlbums" $
    publicly (getTopAlbums <*> tag "depressive" <* limit 2)
   `shouldHaveXml`
    root.node "topalbums".node "album".node "url".text

  it "getTopArtists" $
    publicly (getTopArtists <*> tag "depressive" <* limit 3)
   `shouldHaveXml`
    root.node "topartists".node "artist".node "url".text

  it "getTopTags" $
    publicly getTopTags
   `shouldHaveXml`
    root.node "toptags".node "tag".node "name".text

  it "getTopTracks" $
    publicly (getTopTracks <*> tag "depressive" <* limit 2)
   `shouldHaveXml`
    root.node "toptracks".node "track".node "url".text

  it "getWeeklyArtistChart" $
    publicly (getWeeklyArtistChart <*> tag "depressive" <* limit 3)
   `shouldHaveXml`
    root.node "weeklyartistchart".node "artist".node "name".text

  it "getWeeklyChartList" $
    publicly (getWeeklyChartList <*> tag "depressive")
   `shouldHaveXml`
    root.node "weeklychartlist".node "chart".attr "from"

  it "search" $
    publicly (search <*> tag "depressive" <* limit 3)
   `shouldHaveXml`
    root.node "results".node "tagmatches".node "tag".node "name".text
