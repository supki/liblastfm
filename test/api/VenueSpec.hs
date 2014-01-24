{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module VenueSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Venue
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Venue.getEvents" $
    query ge $
      getEvents <*> venue 9163107 <*> publicKey

  it "Venue.getPastEvents" $
    query gpe $
      getPastEvents <*> venue 9163107 <* limit 2 <*> publicKey

  it "Venue.search" $
    query se $
      search <*> venueName "Arena" <*> publicKey

ge, gpe, se :: Query Text
ge  = key "events".key "event".values.key "venue".key "name"._String
gpe = key "events".key "event".values.key "title"._String
se  = key "results".key "venuematches".key "venue".values.key "id"._String
