{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Album (auth, noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Album
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Album.addTags" testAddTags
  , testCase "Album.getTags-authenticated" testGetTagsAuth
  , testCase "Album.removeTag" testRemoveTag
  , testCase "Album.share" testShare
  ]
 where
  testAddTags = query ok . sign s $
    addTags <*> artist "Pink Floyd" <*> album "The Wall" <*> tags ["70s", "awesome", "classic"]
      <*> ak <*> sk

  testGetTagsAuth = query gt . sign s $
    getTags <*> artist "Pink Floyd" <*> album "The Wall"
      <*> ak <* sk

  testRemoveTag = query ok . sign s $
    removeTag <*> artist "Pink Floyd" <*> album "The Wall" <*> tag "awesome"
      <*> ak <*> sk

  testShare = query ok . sign s $
    share <*> album "Jerusalem" <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"
      <*> ak <*> sk


noauth :: [Test]
noauth =
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
  testGetBuylinks = query gbl $
    getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> album "The Wall" <*> publicKey
  testGetBuylinks_mbid = query gbl $
    getBuyLinks <*> country "United Kingdom" <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <*> publicKey

  testGetInfo = query gi $
    getInfo <*> artist "Pink Floyd" <*> album "The Wall" <*> publicKey
  testGetInfo_mbid = query gi $
    getInfo <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <*> publicKey

  testGetShouts = query gs $
    getShouts <*> artist "Pink Floyd" <*> album "The Wall" <* limit 7 <*> publicKey
  testGetShouts_mbid = query gs $
    getShouts <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* limit 7 <*> publicKey

  testGetTags = query gt $
    getTags <*> artist "Pink Floyd" <*> album "The Wall" <* user "liblastfm" <*> publicKey
  testGetTags_mbid = query gt $
    getTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* user "liblastfm" <*> publicKey

  testGetTopTags = query gtt $
    getTopTags <*> artist "Pink Floyd" <*> album "The Wall" <*> publicKey
  testGetTopTags_mbid = query gtt $
    getTopTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <*> publicKey

  testSearch = query se $
    search <*> album "wall" <* limit 5 <*> publicKey


gbl, gi, gs, gt, gtt, se :: Query Text
gbl = key "affiliations".key "physicals".key "affiliation".values.key "supplierName"._String
gi  = key "album".key "toptags".key "tag".values.key "name"._String
gs  = key "shouts".key "shout".values.key "body"._String
gt  = key "tags".key "tag".values.key "name"._String
gtt = key "toptags".key "tag".values.key "count"._String
se  = key "results".key "albummatches".key "album".values.key "name"._String
