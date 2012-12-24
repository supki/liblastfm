{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Venue (noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Venue
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


noauth ∷ Request JSON Send APIKey → [Test]
noauth ak =
  [ testCase "Venue.getEvents" testGetEvents
  , testCase "Venue.getPastEvents" testGetPastEvents
  , testCase "Venue.search" testSearch
  ]
 where
  testGetEvents = check ge $
    getEvents <*> venue 9163107 <*> ak

  testGetPastEvents = check gpe $
    getPastEvents <*> venue 9163107 <* limit 2 <*> ak

  testSearch = check se $
    search <*> venueName "Arena" <*> ak


ge, gpe, se ∷ Value → Parser [String]
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\t → (t .: "venue") >>= (.: "name"))
gpe o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "title")
se o = parseJSON o >>= (.: "results") >>= (.: "venuematches") >>= (.: "venue") >>= mapM (.: "id")
