{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Album (auth, noauth) where

import Control.Applicative
import Data.Maybe (isJust)
import Data.Monoid

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Album
import Test.HUnit


p ∷ (Value → Parser b) → Value → Maybe b
p f j = case parse f j of
  Success v → Just v
  _ → Nothing


instance Assertable (Maybe a) where
  assert = assertBool "Cannot parse JSON" . isJust


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ TestLabel "Album.addTags" $ TestCase testAddTags
  , TestLabel "Album.getTags-authenticated" $ TestCase testGetTagsAuth
  , TestLabel "Album.removeTag" $ TestCase testRemoveTag
  , TestLabel "Album.share" $ TestCase testShare
  ]
 where
  testAddTags = assert . Just . lastfm . sign sk s $
    addTags "Pink Floyd" "The Wall" ["70s", "awesome", "classic"] <> apiKey ak <> json

  testGetTagsAuth = assert $
    p gt <$> (lastfm . sign  sk s $ getTags <> artist "Pink Floyd" <> album "The Wall" <> apiKey ak <> json)

  testRemoveTag = assert . Just . lastfm . sign sk s $
    removeTag "Pink Floyd" "The Wall" "awesome" <> apiKey ak <> json

  testShare = assert . Just . lastfm . sign sk s $
    share "Jerusalem" "Sleep" "liblastfm" <> message "Just listen!" <> apiKey ak <> json


noauth ∷ [Test]
noauth =
  [ TestLabel "Album.getBuyLinks" $ TestCase testGetBuylinks
  , TestLabel "Album.getInfo" $ TestCase testGetInfo
  , TestLabel "Album.getShouts" $ TestCase testGetShouts
  , TestLabel "Album.getTags" $ TestCase testGetTags
  , TestLabel "Album.getTopTags" $ TestCase testGetTopTags
  , TestLabel "Album.search" $ TestCase testSearch
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetBuylinks = assert $
    p gbl <$> lastfm (getBuyLinks "United Kingdom" <> artist "Pink Floyd" <> album "The Wall" <> apiKey ak <> json)

  testGetInfo = assert $
    p gi <$> lastfm (getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey ak <> json)

  testGetShouts = assert $
    p gs <$> lastfm (getShouts <> artist "Pink Floyd" <> album "The Wall" <> limit 7 <> apiKey ak <> json)

  testGetTags = assert $
    p gt <$> lastfm (getTags <> artist "Pink Floyd" <> album "The Wall" <> user "liblastfm" <> apiKey ak <> json)

  testGetTopTags = assert $
    p gtt <$> lastfm (getTopTags <> artist "Pink Floyd" <> album "The Wall" <> apiKey ak <> json)

  testSearch = assert $
    p se <$> lastfm (search "wall" <> limit 5 <> apiKey ak <> json)


gbl, gi, gs, gt, gtt, se ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gi o = parseJSON o >>= (.: "album") >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "count")
se o = parseJSON o >>= (.: "results") >>= (.: "albummatches") >>= (.: "album") >>= mapM (.: "name")
