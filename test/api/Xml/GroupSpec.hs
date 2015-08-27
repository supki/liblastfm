{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.GroupSpec (spec) where

import Lastfm
import Lastfm.Group
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "getHype" $
    publicly (getHype <*> groupname)
   `shouldHaveXml`
    root.node "weeklyartistchart".node "artist".node "mbid".text

  it "getMembers" $
    publicly (getMembers <*> groupname <* limit 10)
   `shouldHaveXml`
    root.node "members".node "user".node "name".text

  it "getWeeklyAlbumChart" $
    publicly (getWeeklyAlbumChart <*> groupname)
   `shouldHaveXml`
    root.node "weeklyalbumchart".node "album".node "playcount".text

  it "getWeeklyArtistChart" $
    publicly (getWeeklyArtistChart <*> groupname)
   `shouldHaveXml`
    root.node "weeklyartistchart".node "artist".node "name".text

  it "getWeeklyChartList" $
    publicly (getWeeklyChartList <*> groupname)
   `shouldHaveXml`
    root.node "weeklychartlist".node "chart".attr "from"

  it "getWeeklyTrackChart" $
    publicly (getWeeklyTrackChart <*> groupname)
   `shouldHaveXml`
    root.node "weeklytrackchart".node "track".node "url".text

groupname :: Request f Group
groupname = group
  "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"
