{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.ChartSpec (spec) where

import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Chart
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  describe "get*Artists" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "artists".node "artist".node "name".text

    it "getHypedArtists" $
      publicly (getHypedArtists <* limit 3)
     `shouldHaveXml`
      xmlQuery

    it "getTopArtists" $
      publicly (getTopArtists <* limit 4)
     `shouldHaveXml`
      xmlQuery

  describe "get*Tracks" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "tracks".node "track".node "name".text

    it "getHypedTracks" $
      publicly (getHypedTracks <* limit 2)
     `shouldHaveXml`
      xmlQuery

    it "getLovedTracks" $
      publicly (getLovedTracks <* limit 3)
     `shouldHaveXml`
      xmlQuery

    it "getTopTracks" $
      publicly (getTopTracks <* limit 2)
     `shouldHaveXml`
      xmlQuery

  it "getTopTags" $
    publicly (getTopTags <* limit 5)
   `shouldHaveXml`
    root.node "tags".node "tag".node "name".text
