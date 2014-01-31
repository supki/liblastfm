{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.VenueSpec (spec) where

import Data.Aeson.Lens
import Network.Lastfm
import Network.Lastfm.Venue
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "getEvents" $
    publicly (getEvents <*> venue 9163107)
   `shouldHaveJson`
    key "events".key "event".values.key "venue".key "name"._String

  it "getPastEvents" $
    publicly (getPastEvents <*> venue 9163107 <* limit 2)
   `shouldHaveJson`
    key "events".key "event".values.key "title"._String

  it "search" $
    publicly (search <*> venueName "Arena")
   `shouldHaveJson`
    key "results".key "venuematches".key "venue".values.key "id"._String
