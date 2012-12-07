{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Album (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Album
import Test.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ TestLabel "Album.addTags" $ TestCase testAddTags
  , TestLabel "Album.getTags-authenticated" $ TestCase testGetTagsAuth
  , TestLabel "Album.removeTag" $ TestCase testRemoveTag
  , TestLabel "Album.share" $ TestCase testShare
  ]
 where
  testAddTags = assert $ parse ok <:> lastfm (sign s $
    addTags "Pink Floyd" "The Wall" ["70s", "awesome", "classic"] <> apiKey ak <> sessionKey sk <> json)

  testGetTagsAuth = assert $ parse gt <:> lastfm (sign s $
    getTags "Pink Floyd" "The Wall" <> apiKey ak <> sessionKey sk <> json)

  testRemoveTag = assert $ parse ok <:> lastfm (sign s $
    removeTag "Pink Floyd" "The Wall" "awesome" <> apiKey ak <> sessionKey sk <> json)

  testShare = assert $ parse ok <:> lastfm (sign s $
    share "Jerusalem" "Sleep" "liblastfm" <> message "Just listen!" <> apiKey ak <> sessionKey sk <> json)


noauth ∷ [Test]
noauth =
  [ TestLabel "Album.getBuyLinks" $ TestCase testGetBuylinks
  , TestLabel "Album.getBuyLinks_mbid" $ TestCase testGetBuylinks_mbid
  , TestLabel "Album.getInfo" $ TestCase testGetInfo
  , TestLabel "Album.getInfo_mbid" $ TestCase testGetInfo_mbid
  , TestLabel "Album.getShouts" $ TestCase testGetShouts
  , TestLabel "Album.getShouts_mbid" $ TestCase testGetShouts_mbid
  , TestLabel "Album.getTags" $ TestCase testGetTags
  , TestLabel "Album.getTags_mbid" $ TestCase testGetTags_mbid
  , TestLabel "Album.getTopTags" $ TestCase testGetTopTags
  , TestLabel "Album.getTopTags_mbid" $ TestCase testGetTopTags_mbid
  , TestLabel "Album.search" $ TestCase testSearch
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetBuylinks = assert $ parse gbl <:>
    lastfm (getBuyLinks "Pink Floyd" "The Wall" "United Kingdom" <> apiKey ak <> json)

  testGetBuylinks_mbid = assert $ parse gbl <:>
    lastfm (getBuyLinks_mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" "United Kingdom" <> apiKey ak <> json)

  testGetInfo = assert $ parse gi <:>
    lastfm (getInfo "Pink Floyd" "The Wall" <> apiKey ak <> json)

  testGetInfo_mbid = assert $ parse gi <:>
    lastfm (getInfo_mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <> apiKey ak <> json)

  testGetShouts = assert $ parse gs <:>
    lastfm (getShouts "Pink Floyd" "The Wall" <> limit 7 <> apiKey ak <> json)

  testGetShouts_mbid = assert $ parse gs <:>
    lastfm (getShouts_mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <> limit 7 <> apiKey ak <> json)

  testGetTags = assert $ parse gt <:>
    lastfm (getTags "Pink Floyd" "The Wall" <> user "liblastfm" <> apiKey ak <> json)

  testGetTags_mbid = assert $ parse gt <:>
    lastfm (getTags_mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <> user "liblastfm" <> apiKey ak <> json)

  testGetTopTags = assert $ parse gtt <:>
    lastfm (getTopTags "Pink Floyd" "The Wall" <> apiKey ak <> json)

  testGetTopTags_mbid = assert $ parse gtt <:>
    lastfm (getTopTags_mbid "3a16c04b-922b-35c5-a29b-cbe9111fbe79" <> apiKey ak <> json)

  testSearch = assert $ parse se <:>
    lastfm (search "wall" <> limit 5 <> apiKey ak <> json)


gbl, gi, gs, gt, gtt, se ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gi o = parseJSON o >>= (.: "album") >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "count")
se o = parseJSON o >>= (.: "results") >>= (.: "albummatches") >>= (.: "album") >>= mapM (.: "name")
