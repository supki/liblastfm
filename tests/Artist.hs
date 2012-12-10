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


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ testCase "Artist.addTags" testAddTags
  , testCase "Artist.getTags-authenticated" testGetTagsAuth
  , testCase "Artist.removeTag" testRemoveTag
  , testCase "Artist.share" testShare
  ]
 where
  testAddTags = check ok . sign s $
    addTags "Егор Летов" ["russian", "black metal"] <> apiKey ak <> sessionKey sk

  testGetTagsAuth = check gt . sign s $
    getTags "Егор Летов" <> apiKey ak <> sessionKey sk

  testRemoveTag = check ok . sign s $
    removeTag "Егор Летов" "russian" <> apiKey ak <> sessionKey sk

  testShare = check ok . sign s $
    share "Sleep" "liblastfm" <> message "Just listen!" <> apiKey ak <> sessionKey sk


noauth ∷ [Test]
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
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetCorrection = check gc $
    getCorrection "Meshugah" <> apiKey ak

  testGetEvents = check ge $
    getEvents "Meshuggah" <> limit 2 <> apiKey ak

  testGetEvents_mbid = check ge $
    getEvents_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 2 <> apiKey ak

  testGetInfo = check gin $
    getInfo "Meshuggah" <> apiKey ak

  testGetInfo_mbid = check gin $
    getInfo_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak

  testGetPastEvents = check gpe $
    getPastEvents "Meshuggah" <> autocorrect True <> apiKey ak

  testGetPastEvents_mbid = check gpe $
    getPastEvents_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> autocorrect True <> apiKey ak

  testGetPodcast = check gp $
    getPodcast "Meshuggah" <> apiKey ak

  testGetPodcast_mbid = check gp $
    getPodcast_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak

  testGetShouts = check gs $
    getShouts "Meshuggah" <> limit 5 <> apiKey ak

  testGetShouts_mbid = check gs $
    getShouts_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 5 <> apiKey ak

  testGetSimilar = check gsi $
    getSimilar "Meshuggah" <> limit 3 <> apiKey ak

  testGetSimilar_mbid = check gsi $
    getSimilar_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 3 <> apiKey ak

  testGetTags = check gt $
    getTags "Егор Летов" <> user "liblastfm" <> apiKey ak

  testGetTags_mbid = check gt $
    getTags_mbid "cfb3d32e-d095-4d63-946d-9daf06932180" <> user "liblastfm" <> apiKey ak

  testGetTopAlbums = check gta $
    getTopAlbums "Meshuggah" <> limit 3 <> apiKey ak

  testGetTopAlbums_mbid = check gta $
    getTopAlbums_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 3 <> apiKey ak

  testGetTopFans = check gtf $
    getTopFans "Meshuggah" <> apiKey ak

  testGetTopFans_mbid = check gtf $
    getTopFans_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak

  testGetTopTags = check gtt $
    getTopTags "Meshuggah" <> apiKey ak

  testGetTopTags_mbid = check gtt $
    getTopTags_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak

  testGetTopTracks = check gttr $
    getTopTracks "Meshuggah" <> limit 3 <> apiKey ak

  testGetTopTracks_mbid = check gttr $
    getTopTracks_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 3 <> apiKey ak

  testSearch = check se $
    search "Mesh" <> limit 3 <> apiKey ak


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
