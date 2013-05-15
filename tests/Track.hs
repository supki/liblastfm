{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Track (auth, noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Track
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Request JSON APIKey → Request JSON SessionKey → Secret → [Test]
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
  testAddTags = check ok . sign s $
    addTags <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tags ["60s", "awesome"] <*> ak <*> sk

  testBan = check ok . sign s $
    ban <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testLove = check ok . sign s $
    love <*> artist "Gojira" <*> track "Ocean" <*> ak <*> sk

  testRemoveTag = check ok . sign s $
    removeTag <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tag "awesome" <*> ak <*> sk

  testShare = check ok . sign s $
    share <*> artist "Led Zeppelin" <*> track "When the Levee Breaks" <*> recipient "liblastfm" <* message "Just listen!" <*> ak <*> sk

  testUnban = check ok . sign s $
    unban <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testUnlove = check ok . sign s $
    unlove <*> artist "Gojira" <*> track "Ocean" <*> ak <*> sk

  testScrobble = check ss . sign s $
    scrobble <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1300000000 <*> ak <*> sk

  testUpdateNowPlaying = check np . sign s $
    updateNowPlaying <*> artist "Gojira" <*> track "Ocean" <*> ak <*> sk


noauth ∷ Request JSON APIKey → [Test]
noauth ak =
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
  testGetBuylinks = check gbl $
    getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> track "Brain Damage" <*> ak

  testGetCorrection = check gc $
    getCorrection <*> artist "Pink Ployd" <*> track "Brain Damage" <*> ak

  testGetFingerprintMetadata = check gfm $
    getFingerprintMetadata <*> fingerprint 1234 <*> ak

  testGetInfo = check gi $
    getInfo <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* username "aswalrus" <*> ak
  testGetInfo_mbid = check gi $
    getInfo <*> mbid "b6581c74-0b8c-4981-ab92-7eed0298a4bb" <* username "aswalrus" <*> ak

  testGetShouts = check gsh $
    getShouts <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 7 <*> ak
  testGetShouts_mbid = check gsh $
    getShouts <*> mbid "b6581c74-0b8c-4981-ab92-7eed0298a4bb" <* limit 7 <*> ak

  testGetSimilar = check gsi $
    getSimilar <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 4 <*> ak
  testGetSimilar_mbid = check gsi $
    getSimilar <*> mbid "b6581c74-0b8c-4981-ab92-7eed0298a4bb" <* limit 4 <*> ak

  testGetTags = check gt $
    getTags <*> artist "Jefferson Airplane" <*> track "White Rabbit" <* user "liblastfm" <*> ak
  testGetTags_mbid = check gt $
    getTags <*> mbid "1fc619ee-c612-4b2a-a8dc-bb8f1b8b2d6d"  <* user "liblastfm" <*> ak

  testGetTopFans = check gtf $
    getTopFans <*> artist "Pink Floyd" <*> track "Comfortably Numb" <*> ak
  testGetTopFans_mbid = check gtf $
    getTopFans <*> mbid "b6581c74-0b8c-4981-ab92-7eed0298a4bb" <*> ak

  testGetTopTags = check gtt $
    getTopTags <*> artist "Pink Floyd" <*> track "Comfortably Numb" <*> ak
  testGetTopTags_mbid = check gtt $
    getTopTags <*> mbid "b6581c74-0b8c-4981-ab92-7eed0298a4bb" <*> ak

  testSearch = check s' $
    search <*> track "Believe" <* limit 12 <*> ak


gc, gi, gt, ss, np ∷ Value → Parser String
gbl, gfm, gsh, gsi{-, gta-}, gtf, gtt, s' ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "downloads") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gc o = parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "track") >>= (.: "artist") >>= (.: "name")
gfm o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
gi o = parseJSON o >>= (.: "track") >>= (.: "userplaycount")
gsh o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "author")
gsi o = parseJSON o >>= (.: "similartracks") >>= (.: "track") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tags") >>= (.: "@attr") >>= (.: "track")
gtf o = parseJSON o >>= (.: "topfans") >>= (.: "user") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
s' o = parseJSON o >>= (.: "results") >>= (.: "trackmatches") >>= (.: "track") >>= mapM (.: "name")
ss o = parseJSON o >>= (.: "scrobbles") >>= (.: "scrobble") >>= (.: "track") >>= (.: "#text")
np o = parseJSON o >>= (.: "nowplaying") >>= (.: "track") >>= (.: "#text")
