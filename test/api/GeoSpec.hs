{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module GeoSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Geo
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Geo.getEvents" $
    query ge $
      getEvents <* location "Moscow" <* limit 5 <*> publicKey

  it "Geo.getMetroArtistChart" $
    query ga $
      getMetroArtistChart <*> metro "Saint Petersburg" <*> country "Russia" <*> publicKey

  it "Geo.getMetroHypeArtistChart" $
    query ga $
      getMetroHypeArtistChart <*> metro "New York" <*> country "United States" <*> publicKey

  it "Geo.getMetroHypeTrackChart" $
    query gt $
      getMetroHypeTrackChart <*> metro "Moscow" <*> country "Russia" <*> publicKey

  it "Geo.getMetroTrackChart" $
    query gt $
      getMetroTrackChart <*> metro "Boston" <*> country "United States" <*> publicKey

  it "Geo.getMetroUniqueArtistChart" $
    query ga $
      getMetroUniqueArtistChart <*> metro "Minsk" <*> country "Belarus" <*> publicKey

  it "Geo.getMetroUniqueTrackChart" $
    query gt $
      getMetroUniqueTrackChart <*> metro "Moscow" <*> country "Russia" <*> publicKey

  it "Geo.getMetroWeeklyChartlist" $
    query gc $
      getMetroWeeklyChartlist <*> metro "Moscow" <*> publicKey

  it "Geo.getMetros" $
    query gm $
      getMetros <* country "Russia" <*> publicKey

  it "Geo.getTopArtists" $
    query ga $
      getTopArtists <*> country "Belarus" <* limit 3 <*> publicKey

  it "Geo.getTopTracks" $
    query gt $
      getTopTracks <*> country "Ukraine" <* limit 2 <*> publicKey

ga, gc, ge, gm, gt :: Query Text
ga = key "topartists".key "artist".values.key "name"._String
gc = key "weeklychartlist".key "chart".values.key "from"._String
ge = key "events".key "event".values.key "id"._String
gm = key "metros".key "metro".values.key "name"._String
gt = key "toptracks".key "track".values.key "name"._String
