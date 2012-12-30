{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Artist (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Artist
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Request JSON Sign APIKey → Request JSON Sign SessionKey → Text → [Test]
auth ak sk s =
  [ testCase "Artist.addTags" testAddTags
  , testCase "Artist.getTags-authenticated" testGetTagsAuth
  , testCase "Artist.removeTag" testRemoveTag
  , testCase "Artist.share" testShare
  ]
 where
  testAddTags = check ok . sign s $
    addTags <*> artist "Егор Летов" <*> tags ["russian", "black metal"] <*> ak <*> sk

  testGetTagsAuth = check gt . sign s $
    getTags <*> artist "Егор Летов" <*> ak <* sk

  testRemoveTag = check ok . sign s $
    removeTag <*> artist "Егор Летов" <*> tag "russian" <*> ak <*> sk

  testShare = check ok . sign s $
    share <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!" <*> ak <*> sk


noauth ∷ Request JSON Send APIKey → [Test]
noauth ak =
  [ testCase "Artist.getCorrection" testGetCorrection
  , testCase "Artist.getEvents" testGetEvents
  , testCase "Artist.getEvents_mbid" testGetEvents_mbid
  , testCase "Artist.getInfo" testGetInfo
  , testCase "Artist.getInfo_mbid" testGetInfo_mbid
  , testCase "Artist.getPastEvents" testGetPastEvents
  , testCase "Artist.getPastEvents_mbid" testGetPastEvents_mbid
  , testCase "Artist.getPodcast" testGetPodcast
  , testCase "Artist.getPodcast_mbid" testGetPodcast_mbid
  , testCase "Artist.getShouts" testGetShouts
  , testCase "Artist.getShouts_mbid" testGetShouts_mbid
  , testCase "Artist.getSimilar" testGetSimilar
  , testCase "Artist.getSimilar_mbid" testGetSimilar_mbid
  , testCase "Artist.getTags" testGetTags
  , testCase "Artist.getTags_mbid" testGetTags_mbid
  , testCase "Artist.getTopAlbums" testGetTopAlbums
  , testCase "Artist.getTopAlbums_mbid" testGetTopAlbums_mbid
  , testCase "Artist.getTopFans" testGetTopFans
  , testCase "Artist.getTopFans_mbid" testGetTopFans_mbid
  , testCase "Artist.getTopTags" testGetTopTags
  , testCase "Artist.getTopTags_mbid" testGetTopTags_mbid
  , testCase "Artist.getTopTracks" testGetTopTracks
  , testCase "Artist.getTopTracks_mbid" testGetTopTracks_mbid
  , testCase "Artist.search" testSearch
  ]
 where
  testGetCorrection = check gc $
    getCorrection <*> artist "Meshugah" <*> ak

  testGetEvents = check ge $
    getEvents <*> artist "Meshuggah" <* limit 2 <*> ak
  testGetEvents_mbid = check ge $
    getEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 2 <*> ak

  testGetInfo = check gin $
    getInfo <*> artist "Meshuggah" <*> ak
  testGetInfo_mbid = check gin $
    getInfo <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> ak

  testGetPastEvents = check gpe $
    getPastEvents <*> artist "Meshuggah" <* autocorrect True <*> ak
  testGetPastEvents_mbid = check gpe $
    getPastEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* autocorrect True <*> ak

  testGetPodcast = check gp $
    getPodcast <*> artist "Meshuggah" <*> ak
  testGetPodcast_mbid = check gp $
    getPodcast <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> ak

  testGetShouts = check gs $
    getShouts <*> artist "Meshuggah" <* limit 5 <*> ak
  testGetShouts_mbid = check gs $
    getShouts <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 5 <*> ak

  testGetSimilar = check gsi $
    getSimilar <*> artist "Meshuggah" <* limit 3 <*> ak
  testGetSimilar_mbid = check gsi $
    getSimilar <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> ak

  testGetTags = check gt $
    getTags <*> artist "Егор Летов" <* user "liblastfm" <*> ak
  testGetTags_mbid = check gt $
    getTags <*> mbid "cfb3d32e-d095-4d63-946d-9daf06932180" <* user "liblastfm" <*> ak

  testGetTopAlbums = check gta $
    getTopAlbums <*> artist "Meshuggah" <* limit 3 <*> ak
  testGetTopAlbums_mbid = check gta $
    getTopAlbums <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> ak

  testGetTopFans = check gtf $
    getTopFans <*> artist "Meshuggah" <*> ak
  testGetTopFans_mbid = check gtf $
    getTopFans <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> ak

  testGetTopTags = check gtt $
    getTopTags <*> artist "Meshuggah" <*> ak
  testGetTopTags_mbid = check gtt $
    getTopTags <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> ak

  testGetTopTracks = check gttr $
    getTopTracks <*> artist "Meshuggah" <* limit 3 <*> ak
  testGetTopTracks_mbid = check gttr $
    getTopTracks <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> ak

  testSearch = check se $
    search <*> artist "Mesh" <* limit 3 <*> ak


gc, gin, gp ∷ Value → Parser String
ge, gpe, gs, gsi, gt, gta, gtf, gtt, gttr, se ∷ Value → Parser [String]
gc o = parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "artist") >>= (.: "name")
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\o' → (o' .: "venue") >>= (.: "name"))
gin o = parseJSON o >>= (.: "artist") >>= (.: "stats") >>= (.: "listeners")
gp o = parseJSON o >>= (.: "rss") >>= (.: "channel") >>= (.: "description")
gpe o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "title")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "author")
gsi o = parseJSON o >>= (.: "similarartists") >>= (.: "artist") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gta o = parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "name")
gtf o = parseJSON o >>= (.: "topfans") >>= (.: "user") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gttr o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name")
se o = parseJSON o >>= (.: "results") >>= (.: "artistmatches") >>= (.: "artist") >>= mapM (.: "name")
