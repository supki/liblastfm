{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module GroupSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Group
import Test.Hspec

import SpecHelper

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


spec :: Spec
spec = do
  it "Group.getHype" $
    query gh $
      getHype <*> group group_name <*> publicKey

  it "Group.getMembers" $
    query gm $
      getMembers <*> group group_name <* limit 10 <*> publicKey

  it "Group.getWeeklyAlbumChart" $
    query ga $
      getWeeklyAlbumChart <*> group group_name <*> publicKey

  it "Group.getWeeklyArtistChart" $
    query gar $
      getWeeklyArtistChart <*> group group_name <*> publicKey

  it "Group.getWeeklyChartList" $
    query gc $
      getWeeklyChartList <*> group group_name <*> publicKey

  it "Group.getWeeklyTrackChart" $
    query gt $
      getWeeklyTrackChart <*> group group_name <*> publicKey

group_name :: Text
group_name = "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"

ga, gar, gc, gh, gm, gt :: Query Text
ga  = key "weeklyalbumchart".key "album".values.key "playcount"._String
gar = key "weeklyartistchart".key "artist".values.key "name"._String
gc  = key "weeklychartlist".key "chart".values.key "from"._String
gh  = key "weeklyartistchart".key "artist".values.key "mbid"._String
gm  = key "members".key "user".values.key "name"._String
gt  = key "weeklytrackchart".key "track".values.key "url"._String
