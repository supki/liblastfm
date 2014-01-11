{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Track (auth, noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Track
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Track.addTags" testAddTags
  , testCase "Track.ban" testBan
  , testCase "Track.love" testLove
  , testCase "Track.removeTag" testRemoveTag
  , testCase "Track.share" testShare
  , testCase "Track.unban" testUnban
  , testCase "Track.unlove" testUnlove
  , testCase "Track.scrobble" testScrobble
  , testCase "Track.updateNowPlaying" testUpdateNowPlaying
  ]
 where
  testAddTags = query ok . sign s $
    addTags <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tags ["60s", "awesome"] <*> ak <*> sk

  testBan = query ok . sign s $
    ban <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testLove = query ok . sign s $
    love <*> artist "Gojira" <*> track "Ocean" <*> ak <*> sk

  testRemoveTag = query ok . sign s $
    removeTag <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tag "awesome" <*> ak <*> sk

  testShare = query ok . sign s $
    share <*> artist "Led Zeppelin" <*> track "When the Levee Breaks" <*> recipient "liblastfm" <* message "Just listen!" <*> ak <*> sk

  testUnban = query ok . sign s $
    unban <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testUnlove = query ok . sign s $
    unlove <*> artist "Gojira" <*> track "Ocean" <*> ak <*> sk

  testScrobble = query ss . sign s $
    scrobble (pure (item <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1300000000)) <*> ak <*> sk

  testUpdateNowPlaying = query np . sign s $
    updateNowPlaying <*> artist "Gojira" <*> track "Ocean" <*> ak <*> sk


noauth :: [Test]
noauth =
  [ testCase "Track.getBuylinks" testGetBuylinks
  , testCase "Track.getCorrection" testGetCorrection
  , testCase "Track.getFingerprintMetadata" testGetFingerprintMetadata
  , testCase "Track.getInfo" testGetInfo
  , testCase "Track.getInfo_mbid" testGetInfo_mbid
  , testCase "Track.getShouts" testGetShouts
  , testCase "Track.getShouts_mbid" testGetShouts_mbid
  , testCase "Track.getSimilar" testGetSimilar
  , testCase "Track.getSimilar_mbid" testGetSimilar_mbid
  , testCase "Track.getTags" testGetTags
  , testCase "Track.getTags_mbid" testGetTags_mbid
  , testCase "Track.getTopFans" testGetTopFans
  , testCase "Track.getTopFans_mbid" testGetTopFans_mbid
  , testCase "Track.getTopTags" testGetTopTags
  , testCase "Track.getTopTags_mbid" testGetTopTags_mbid
  , testCase "Track.search" testSearch
  ]
 where
  testGetBuylinks = query gbl $
    getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> track "Brain Damage" <*> publicKey

  testGetCorrection = query gc $
    getCorrection <*> artist "Pink Ployd" <*> track "Brain Damage" <*> publicKey

  testGetFingerprintMetadata = query gfm $
    getFingerprintMetadata <*> fingerprint 1234 <*> publicKey

  testGetInfo = query gi $
    getInfo <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* username "aswalrus" <*> publicKey
  testGetInfo_mbid = query gi $
    getInfo <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* username "aswalrus" <*> publicKey

  testGetShouts = query gsh $
    getShouts <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 7 <*> publicKey
  testGetShouts_mbid = query gsh $
    getShouts <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 7 <*> publicKey

  testGetSimilar = query gsi $
    getSimilar <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 4 <*> publicKey
  testGetSimilar_mbid = query gsi $
    getSimilar <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 4 <*> publicKey

  testGetTags = query gt $
    getTags <*> artist "Jefferson Airplane" <*> track "White Rabbit" <* user "liblastfm" <*> publicKey
  testGetTags_mbid = query gt $
    getTags <*> mbid "001b3337-faf4-421a-a11f-45e0b60a7703"  <* user "liblastfm" <*> publicKey

  testGetTopFans = query gtf $
    getTopFans <*> artist "Pink Floyd" <*> track "Comfortably Numb" <*> publicKey
  testGetTopFans_mbid = query gtf $
    getTopFans <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <*> publicKey

  testGetTopTags = query gtt $
    getTopTags <*> artist "Pink Floyd" <*> track "Comfortably Numb" <*> publicKey
  testGetTopTags_mbid = query gtt $
    getTopTags <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <*> publicKey

  testSearch = query s' $
    search <*> track "Believe" <* limit 12 <*> publicKey


gbl, gc, gfm, gi, gsh, gsi, gt, gtf, gtt, np, s', ss :: Query Text
gbl = key "affiliations".key "downloads".key "affiliation".values.key "supplierName"._String
gc  = key "corrections".key "correction".key "track".key "artist".key "name"._String
gfm = key "tracks".key "track".values.key "name"._String
gi  = key "track".key "userplaycount"._String
gsh = key "shouts".key "shout".values.key "author"._String
gsi = key "similartracks".key "track".values.key "name"._String
gt  = key "tags".key "@attr".key "track"._String
gtf = key "topfans".key "user".values.key "name"._String
gtt = key "toptags".key "tag".values.key "name"._String
s'  = key "results".key "trackmatches".key "track".values.key "name"._String
ss  = key "scrobbles".key "scrobble".key "track".key "#text"._String
np  = key "nowplaying".key "track".key "#text"._String
