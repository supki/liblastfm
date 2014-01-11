{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Venue (noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Venue
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


noauth :: [Test]
noauth =
  [ testCase "Venue.getEvents" testGetEvents
  , testCase "Venue.getPastEvents" testGetPastEvents
  , testCase "Venue.search" testSearch
  ]
 where
  testGetEvents = query ge $
    getEvents <*> venue 9163107 <*> publicKey

  testGetPastEvents = query gpe $
    getPastEvents <*> venue 9163107 <* limit 2 <*> publicKey

  testSearch = query se $
    search <*> venueName "Arena" <*> publicKey


ge, gpe, se :: Query Text
ge  = key "events".key "event".values.key "venue".key "name"._String
gpe = key "events".key "event".values.key "title"._String
se  = key "results".key "venuematches".key "venue".values.key "id"._String
