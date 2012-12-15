{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Album (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Album
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ testCase "Album.addTags" testAddTags
  , testCase "Album.getTags-authenticated" testGetTagsAuth
  , testCase "Album.removeTag" testRemoveTag
  , testCase "Album.share" testShare
  ]
 where
  testAddTags = check ok . sign s $
    addTags <*> artist "Pink Floyd" <*> album "The Wall" <*> tags ["70s", "awesome", "classic"]
      <*> apiKey ak <*> sessionKey sk

  testGetTagsAuth = check gt . sign s $
    getTags <*> artist "Pink Floyd" <*> album "The Wall"
      <*> apiKey ak <* sessionKey sk

  testRemoveTag = check ok . sign s $
    removeTag <*> artist "Pink Floyd" <*> album "The Wall" <*> tag "awesome"
      <*> apiKey ak <*> sessionKey sk

  testShare = check ok . sign s $
    share <*> album "Jerusalem" <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"
      <*> apiKey ak <*> sessionKey sk


noauth ∷ [Test]
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
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetBuylinks = check gbl $
    getBuyLinks <*> artist "Pink Floyd" <*> album "The Wall" <*> country "United Kingdom"
      <*> apiKey ak

  testGetBuylinks_mbid = check gbl $
    getBuyLinks_mbid <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <*> country "United Kingdom"
      <*> apiKey ak

  testGetInfo = check gi $
    getInfo <*> artist "Pink Floyd" <*> album "The Wall"
      <*> apiKey ak

  testGetInfo_mbid = check gi $
    getInfo_mbid <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79"
      <*> apiKey ak

  testGetShouts = check gs $
    getShouts <*> artist "Pink Floyd" <*> album "The Wall" <* limit 7
      <*> apiKey ak

  testGetShouts_mbid = check gs $
    getShouts_mbid <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <* limit 7
      <*> apiKey ak

  testGetTags = check gt $
    getTags <*> artist "Pink Floyd" <*> album "The Wall" <* user "liblastfm"
      <*> apiKey ak

  testGetTags_mbid = check gt $
    getTags_mbid <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <* user "liblastfm"
      <*> apiKey ak

  testGetTopTags = check gtt $
    getTopTags <*> artist "Pink Floyd" <*> album "The Wall"
      <*> apiKey ak

  testGetTopTags_mbid = check gtt $
    getTopTags_mbid <*> mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79"
      <*> apiKey ak

  testSearch = check se $
    search <*> album "wall" <* limit 5 <* apiKey ak


gbl, gi, gs, gt, gtt, se ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gi o = parseJSON o >>= (.: "album") >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "count")
se o = parseJSON o >>= (.: "results") >>= (.: "albummatches") >>= (.: "album") >>= mapM (.: "name")
