{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.ChartSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Chart
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  describe "get*Artists" $ do
    let jsonQuery :: Fold Value Text
        jsonQuery = key "artists".key "artist".values.key "name"._String

    it "getHypedArtists" $
      publicly (getHypedArtists <* limit 3)
     `shouldHaveJson`
      jsonQuery

    it "getTopArtists" $
      publicly (getTopArtists <* limit 4)
     `shouldHaveJson`
      jsonQuery

  describe "get*Tracks" $ do
    let jsonQuery :: Fold Value Text
        jsonQuery = key "tracks".key "track".values.key "name"._String

    it "getHypedTracks" $
      publicly (getHypedTracks <* limit 2)
     `shouldHaveJson`
      jsonQuery

    it "getLovedTracks" $
      publicly (getLovedTracks <* limit 3)
     `shouldHaveJson`
      jsonQuery

    it "getTopTracks" $
      publicly (getTopTracks <* limit 2)
     `shouldHaveJson`
      jsonQuery

  it "getTopTags" $
    publicly (getTopTags <* limit 5)
   `shouldHaveJson`
    key "tags".key "tag".values.key "name"._String
