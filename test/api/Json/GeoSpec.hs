{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.GeoSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Geo
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "getEvents" $
    publicly (getEvents <* location "Moscow" <* limit 5)
   `shouldHaveJson`
    key "events".key "event".values.key "id"._String

  describe "get*Artist*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "topartists".key "artist".values.key "name"._String

    it "getMetroArtistChart" $
      publicly (getMetroArtistChart <*> metro "Saint Petersburg" <*> country "Russia")
     `shouldHaveJson`
      jsonQuery

    it "getMetroHypeArtistChart" $
      publicly (getMetroHypeArtistChart <*> metro "New York" <*> country "United States")
     `shouldHaveJson`
      jsonQuery

    it "getMetroUniqueArtistChart" $
      publicly (getMetroUniqueArtistChart <*> metro "Minsk" <*> country "Belarus")
     `shouldHaveJson`
      jsonQuery

    it "getTopArtists" $
      publicly (getTopArtists <*> country "Belarus" <* limit 3)
     `shouldHaveJson`
      jsonQuery

  it "getMetroWeeklyChartlist" $
    publicly (getMetroWeeklyChartlist <*> metro "Moscow")
   `shouldHaveJson`
    key "weeklychartlist".key "chart".values.key "from"._String

  it "getMetros" $
    publicly (getMetros <* country "Russia")
   `shouldHaveJson`
    key "metros".key "metro".values.key "name"._String

  describe "get*Track*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "toptracks".key "track".values.key "name"._String

    it "getTopTracks" $
      publicly (getTopTracks <*> country "Ukraine" <* limit 2)
     `shouldHaveJson`
      jsonQuery

    it "getMetroUniqueTrackChart" $
      publicly (getMetroUniqueTrackChart <*> metro "Moscow" <*> country "Russia")
     `shouldHaveJson`
      jsonQuery

    it "getMetroHypeTrackChart" $
      publicly (getMetroHypeTrackChart <*> metro "Moscow" <*> country "Russia")
     `shouldHaveJson`
      jsonQuery

    it "getMetroTrackChart" $
      publicly (getMetroTrackChart <*> metro "Boston" <*> country "United States")
     `shouldHaveJson`
      jsonQuery
