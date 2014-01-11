{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Artist (auth, noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Artist
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Artist.addTags" testAddTags
  , testCase "Artist.getTags-authenticated" testGetTagsAuth
  , testCase "Artist.removeTag" testRemoveTag
  , testCase "Artist.share" testShare
  ]
 where
  testAddTags = query ok . sign s $
    addTags <*> artist "Егор Летов" <*> tags ["russian", "black metal"] <*> ak <*> sk

  testGetTagsAuth = query gt . sign s $
    getTags <*> artist "Егор Летов" <*> ak <* sk

  testRemoveTag = query ok . sign s $
    removeTag <*> artist "Егор Летов" <*> tag "russian" <*> ak <*> sk

  testShare = query ok . sign s $
    share <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!" <*> ak <*> sk


noauth :: [Test]
noauth =
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
  testGetCorrection = query gc $
    getCorrection <*> artist "Meshugah" <*> publicKey

  testGetEvents = query ge $
    getEvents <*> artist "Meshuggah" <* limit 2 <*> publicKey
  testGetEvents_mbid = query ge $
    getEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 2 <*> publicKey

  testGetInfo = query gin $
    getInfo <*> artist "Meshuggah" <*> publicKey
  testGetInfo_mbid = query gin $
    getInfo <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  testGetPastEvents = query gpe $
    getPastEvents <*> artist "Meshuggah" <* autocorrect True <*> publicKey
  testGetPastEvents_mbid = query gpe $
    getPastEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* autocorrect True <*> publicKey

  testGetPodcast = query gp $
    getPodcast <*> artist "Meshuggah" <*> publicKey
  testGetPodcast_mbid = query gp $
    getPodcast <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  testGetShouts = query gs $
    getShouts <*> artist "Meshuggah" <* limit 5 <*> publicKey
  testGetShouts_mbid = query gs $
    getShouts <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 5 <*> publicKey

  testGetSimilar = query gsi $
    getSimilar <*> artist "Meshuggah" <* limit 3 <*> publicKey
  testGetSimilar_mbid = query gsi $
    getSimilar <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> publicKey

  testGetTags = query gt $
    getTags <*> artist "Егор Летов" <* user "liblastfm" <*> publicKey
  testGetTags_mbid = query gt $
    getTags <*> mbid "cfb3d32e-d095-4d63-946d-9daf06932180" <* user "liblastfm" <*> publicKey

  testGetTopAlbums = query gta $
    getTopAlbums <*> artist "Meshuggah" <* limit 3 <*> publicKey
  testGetTopAlbums_mbid = query gta $
    getTopAlbums <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> publicKey

  testGetTopFans = query gtf $
    getTopFans <*> artist "Meshuggah" <*> publicKey
  testGetTopFans_mbid = query gtf $
    getTopFans <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  testGetTopTags = query gtt $
    getTopTags <*> artist "Meshuggah" <*> publicKey
  testGetTopTags_mbid = query gtt $
    getTopTags <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  testGetTopTracks = query gttr $
    getTopTracks <*> artist "Meshuggah" <* limit 3 <*> publicKey
  testGetTopTracks_mbid = query gttr $
    getTopTracks <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> publicKey

  testSearch = query se $
    search <*> artist "Mesh" <* limit 3 <*> publicKey



gc, gin, gp :: Query Text
ge, gpe, gs, gsi, gt, gta, gtf, gtt, gttr, se :: Query Text
gc   = key "corrections".key "correction".key "artist".key "name"._String
ge   = key "events".key "artist"._String
gin  = key "artist".key "stats".key "listeners"._String
gp   = key "rss".key "channel".key "description"._String
gpe  = key "events".key "event".values.key "title"._String
gs   = key "shouts".key "shout".values.key "author"._String
gsi  = key "similarartists".key "artist".values.key "name"._String
gt   = key "tags".key "tag".values.key "name"._String
gta  = key "topalbums".key "album".values.key "name"._String
gtf  = key "topfans".key "user".values.key "name"._String
gtt  = key "toptags".key "tag".values.key "name"._String
gttr = key "toptracks".key "track".values.key "name"._String
se   = key "results".key "artistmatches".key "artist".values.key "name"._String
