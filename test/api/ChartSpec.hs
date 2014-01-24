{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module ChartSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Chart
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Chart.getHypedArtists" $
    query ga $
      getHypedArtists <* limit 3 <*> publicKey

  it "Chart.getHypedTracks" $
    query gt $
      getHypedTracks <* limit 2 <*> publicKey

  it "Chart.getLovedTracks" $
    query gt $
      getLovedTracks <* limit 3 <*> publicKey

  it "Chart.getTopArtists" $
    query ga $
      getTopArtists <* limit 4 <*> publicKey

  it "Chart.getTopTags" $
    query gta $
      getTopTags <* limit 5 <*> publicKey

  it "Chart.getTopTracks" $
    query gt $
      getTopTracks <* limit 2 <*> publicKey

ga, gt, gta :: Query Text
ga  = key "artists".key "artist".values.key "name"._String
gt  = key "tracks".key "track".values.key "name"._String
gta = key "tags".key "tag".values.key "name"._String
