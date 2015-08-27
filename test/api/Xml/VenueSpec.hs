{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.VenueSpec (spec) where

import Lastfm
import Lastfm.Venue
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "getEvents" $
    publicly (getEvents <*> venue 9163107)
   `shouldHaveXml`
    root.node "events".node "event".node "venue".node "name".text

  it "getPastEvents" $
    publicly (getPastEvents <*> venue 9163107 <* limit 2)
   `shouldHaveXml`
    root.node "events".node "event".node "title".text

  it "search" $
    publicly (search <*> venueName "Arena")
   `shouldHaveXml`
    root.node "results".node "venuematches".node "venue".node "id".text
