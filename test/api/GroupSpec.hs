{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module GroupSpec (spec) where

import Control.Lens.Aeson
import Network.Lastfm
import Network.Lastfm.Group
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "getHype" $
    publicly (getHype <*> groupname)
   `shouldHaveJson`
    key "weeklyartistchart".key "artist".values.key "mbid"._String

  it "getMembers" $
    publicly (getMembers <*> groupname <* limit 10)
   `shouldHaveJson`
    key "members".key "user".values.key "name"._String

  it "getWeeklyAlbumChart" $
    publicly (getWeeklyAlbumChart <*> groupname)
   `shouldHaveJson`
    key "weeklyalbumchart".key "album".values.key "playcount"._String

  it "getWeeklyArtistChart" $
    publicly (getWeeklyArtistChart <*> groupname)
   `shouldHaveJson`
    key "weeklyartistchart".key "artist".values.key "name"._String

  it "getWeeklyChartList" $
    publicly (getWeeklyChartList <*> groupname)
   `shouldHaveJson`
    key "weeklychartlist".key "chart".values.key "from"._String

  it "getWeeklyTrackChart" $
    publicly (getWeeklyTrackChart <*> groupname)
   `shouldHaveJson`
    key "weeklytrackchart".key "track".values.key "url"._String

groupname :: Request f Group
groupname = group
  "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"
