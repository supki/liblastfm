{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.GeoSpec (spec) where

import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Geo
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "getEvents" $
    publicly (getEvents <* location "Moscow" <* limit 5)
   `shouldHaveXml`
    root.node "events".node "event".node "id".text

  describe "get*Artist*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "topartists".node "artist".node "name".text

    it "getMetroArtistChart" $
      publicly (getMetroArtistChart <*> metro "Saint Petersburg" <*> country "Russia")
     `shouldHaveXml`
      xmlQuery

    it "getMetroHypeArtistChart" $
      publicly (getMetroHypeArtistChart <*> metro "New York" <*> country "United States")
     `shouldHaveXml`
      xmlQuery

    it "getMetroUniqueArtistChart" $
      publicly (getMetroUniqueArtistChart <*> metro "Minsk" <*> country "Belarus")
     `shouldHaveXml`
      xmlQuery

    it "getTopArtists" $
      publicly (getTopArtists <*> country "Belarus" <* limit 3)
     `shouldHaveXml`
      xmlQuery

  it "getMetroWeeklyChartlist" $
    publicly (getMetroWeeklyChartlist <*> metro "Moscow")
   `shouldHaveXml`
    root.node "weeklychartlist".node "chart".attr "from"

  it "getMetros" $
    publicly (getMetros <* country "Russia")
   `shouldHaveXml`
    root.node "metros".node "metro".node "name".text

  describe "get*Track*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "toptracks".node "track".node "name".text

    it "getTopTracks" $
      publicly (getTopTracks <*> country "Ukraine" <* limit 2)
     `shouldHaveXml`
      xmlQuery

    it "getMetroUniqueTrackChart" $
      publicly (getMetroUniqueTrackChart <*> metro "Moscow" <*> country "Russia")
     `shouldHaveXml`
      xmlQuery

    it "getMetroHypeTrackChart" $
      publicly (getMetroHypeTrackChart <*> metro "Moscow" <*> country "Russia")
     `shouldHaveXml`
      xmlQuery

    it "getMetroTrackChart" $
      publicly (getMetroTrackChart <*> metro "Boston" <*> country "United States")
     `shouldHaveXml`
      xmlQuery
