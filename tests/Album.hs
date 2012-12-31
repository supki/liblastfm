{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Album (auth, noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Album
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Request JSON Sign APIKey → Request JSON Sign SessionKey → Secret → [Test]
auth ak sk s =
  [ testCase "Album.addTags" testAddTags
  , testCase "Album.getTags-authenticated" testGetTagsAuth
  , testCase "Album.removeTag" testRemoveTag
  , testCase "Album.share" testShare
  ]
 where
  testAddTags = check ok . sign s $
    addTags <*> artist "Pink Floyd" <*> album "The Wall" <*> tags ["70s", "awesome", "classic"]
      <*> ak <*> sk

  testGetTagsAuth = check gt . sign s $
    getTags <*> artist "Pink Floyd" <*> album "The Wall"
      <*> ak <* sk

  testRemoveTag = check ok . sign s $
    removeTag <*> artist "Pink Floyd" <*> album "The Wall" <*> tag "awesome"
      <*> ak <*> sk

  testShare = check ok . sign s $
    share <*> album "Jerusalem" <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"
      <*> ak <*> sk


noauth ∷ Request JSON Send APIKey → [Test]
noauth ak =
  [ testCase "Album.getBuyLinks" testGetBuylinks
  , testCase "Album.getBuyLinks_mbid" testGetBuylinks_mbid
  , testCase "Album.getInfo" testGetInfo
  , testCase "Album.getInfo_mbid" testGetInfo_mbid
  , testCase "Album.getShouts" testGetShouts
  , testCase "Album.getShouts_mbid" testGetShouts_mbid
  , testCase "Album.getTags" testGetTags
  , testCase "Album.getTags_mbid" testGetTags_mbid
  , testCase "Album.getTopTags" testGetTopTags
  , testCase "Album.getTopTags_mbid" testGetTopTags_mbid
  , testCase "Album.search" testSearch
  ]
 where
  testGetBuylinks = check gbl $
    getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> album "The Wall" <*> ak
  testGetBuylinks_mbid = check gbl $
    getBuyLinks <*> country "United Kingdom" <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <*> ak

  testGetInfo = check gi $
    getInfo <*> artist "Pink Floyd" <*> album "The Wall" <*> ak
  testGetInfo_mbid = check gi $
    getInfo <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <*> ak

  testGetShouts = check gs $
    getShouts <*> artist "Pink Floyd" <*> album "The Wall" <* limit 7 <*> ak
  testGetShouts_mbid = check gs $
    getShouts <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <* limit 7 <*> ak

  testGetTags = check gt $
    getTags <*> artist "Pink Floyd" <*> album "The Wall" <* user "liblastfm" <*> ak
  testGetTags_mbid = check gt $
    getTags <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <* user "liblastfm" <*> ak

  testGetTopTags = check gtt $
    getTopTags <*> artist "Pink Floyd" <*> album "The Wall" <*> ak
  testGetTopTags_mbid = check gtt $
    getTopTags <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <*> ak

  testSearch = check se $
    search <*> album "wall" <* limit 5 <*> ak


gbl, gi, gs, gt, gtt, se ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gi o = parseJSON o >>= (.: "album") >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "count")
se o = parseJSON o >>= (.: "results") >>= (.: "albummatches") >>= (.: "album") >>= mapM (.: "name")
