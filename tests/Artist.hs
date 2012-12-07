{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Artist (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Artist
import Test.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ TestLabel "Artist.addTags" $ TestCase testAddTags
  , TestLabel "Artist.getTags-authenticated" $ TestCase testGetTagsAuth
  , TestLabel "Artist.removeTag" $ TestCase testRemoveTag
  , TestLabel "Artist.share" $ TestCase testShare
  ]
 where
  testAddTags = assert $ parse ok <:> lastfm (sign s $
    addTags "Егор Летов" ["russian", "black metal"] <> apiKey ak <> sessionKey sk <> json)

  testGetTagsAuth = assert $ parse gt <:> lastfm (sign s $
    getTags "Егор Летов" <> apiKey ak <> sessionKey sk <> json)

  testRemoveTag = assert $ parse ok <:> lastfm (sign s $
    removeTag "Егор Летов" "russian" <> apiKey ak <> sessionKey sk <> json)

  testShare = assert $ parse ok <:> lastfm (sign s $
    share "Sleep" "liblastfm" <> message "Just listen!" <> apiKey ak <> sessionKey sk <> json)


noauth ∷ [Test]
noauth =
  [ TestLabel "Artist.getCorrection" $ TestCase testGetCorrection
  , TestLabel "Artist.getEvents" $ TestCase testGetEvents
  , TestLabel "Artist.getEvents_mbid" $ TestCase testGetEvents_mbid
  , TestLabel "Artist.getInfo" $ TestCase testGetInfo
  , TestLabel "Artist.getInfo_mbid" $ TestCase testGetInfo_mbid
  , TestLabel "Artist.getPastEvents" $ TestCase testGetPastEvents
  , TestLabel "Artist.getPastEvents_mbid" $ TestCase testGetPastEvents_mbid
  , TestLabel "Artist.getPodcast" $ TestCase testGetPodcast
  , TestLabel "Artist.getPodcast_mbid" $ TestCase testGetPodcast_mbid
  , TestLabel "Artist.getShouts" $ TestCase testGetShouts
  , TestLabel "Artist.getShouts_mbid" $ TestCase testGetShouts_mbid
  , TestLabel "Artist.getSimilar" $ TestCase testGetSimilar
  , TestLabel "Artist.getSimilar_mbid" $ TestCase testGetSimilar_mbid
  , TestLabel "Artist.getTags" $ TestCase testGetTags
  , TestLabel "Artist.getTags_mbid" $ TestCase testGetTags_mbid
  , TestLabel "Artist.getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "Artist.getTopAlbums_mbid" $ TestCase testGetTopAlbums_mbid
  , TestLabel "Artist.getTopFans" $ TestCase testGetTopFans
  , TestLabel "Artist.getTopFans_mbid" $ TestCase testGetTopFans_mbid
  , TestLabel "Artist.getTopTags" $ TestCase testGetTopTags
  , TestLabel "Artist.getTopTags_mbid" $ TestCase testGetTopTags_mbid
  , TestLabel "Artist.getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "Artist.getTopTracks_mbid" $ TestCase testGetTopTracks_mbid
  , TestLabel "Artist.search" $ TestCase testSearch
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetCorrection = assert $ parse gc <:> lastfm (
    getCorrection "Meshugah" <> apiKey ak <> json)

  testGetEvents = assert $ parse ge <:> lastfm (
    getEvents "Meshuggah" <> limit 2 <> apiKey ak <> json)

  testGetEvents_mbid = assert $ parse ge <:> lastfm (
    getEvents_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 2 <> apiKey ak <> json)

  testGetInfo = assert $ parse gin <:> lastfm (
    getInfo "Meshuggah" <> apiKey ak <> json)

  testGetInfo_mbid = assert $ parse gin <:> lastfm (
    getInfo_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak <> json)

  testGetPastEvents = assert $ parse gpe <:> lastfm (
    getPastEvents "Meshuggah" <> autocorrect True <> apiKey ak <> json)

  testGetPastEvents_mbid = assert $ parse gpe <:> lastfm (
    getPastEvents_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> autocorrect True <> apiKey ak <> json)

  testGetPodcast = assert $ parse gp <:> lastfm (
    getPodcast "Meshuggah" <> apiKey ak <> json)

  testGetPodcast_mbid = assert $ parse gp <:> lastfm (
    getPodcast_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak <> json)

  testGetShouts = assert $ parse gs <:> lastfm (
    getShouts "Meshuggah" <> limit 5 <> apiKey ak <> json)

  testGetShouts_mbid = assert $ parse gs <:> lastfm (
    getShouts_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 5 <> apiKey ak <> json)

  testGetSimilar = assert $ parse gsi <:> lastfm (
    getSimilar "Meshuggah" <> limit 3 <> apiKey ak <> json)

  testGetSimilar_mbid = assert $ parse gsi <:> lastfm (
    getSimilar_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 3 <> apiKey ak <> json)

  testGetTags = assert $ parse gt <:> lastfm (
    getTags "Егор Летов" <> user "liblastfm" <> apiKey ak <> json)

  testGetTags_mbid = assert $ parse gt <:> lastfm (
    getTags_mbid "cfb3d32e-d095-4d63-946d-9daf06932180" <> user "liblastfm" <> apiKey ak <> json)

  testGetTopAlbums = assert $ parse gta <:> lastfm (
    getTopAlbums "Meshuggah" <> limit 3 <> apiKey ak <> json)

  testGetTopAlbums_mbid = assert $ parse gta <:> lastfm (
    getTopAlbums_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 3 <> apiKey ak <> json)

  testGetTopFans = assert $ parse gtf <:> lastfm (
    getTopFans "Meshuggah" <> apiKey ak <> json)

  testGetTopFans_mbid = assert $ parse gtf <:> lastfm (
    getTopFans_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak <> json)

  testGetTopTags = assert $ parse gtt <:> lastfm (
    getTopTags "Meshuggah" <> apiKey ak <> json)

  testGetTopTags_mbid = assert $ parse gtt <:> lastfm (
    getTopTags_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> apiKey ak <> json)

  testGetTopTracks = assert $ parse gttr <:> lastfm (
    getTopTracks "Meshuggah" <> limit 3 <> apiKey ak <> json)

  testGetTopTracks_mbid = assert $ parse gttr <:> lastfm (
    getTopTracks_mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <> limit 3 <> apiKey ak <> json)

  testSearch = assert $ parse se <:> lastfm (
    search "Mesh" <> limit 3 <> apiKey ak <> json)


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
