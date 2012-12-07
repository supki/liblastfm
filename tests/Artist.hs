{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  testAddTags = assert $ parse ok <:> lastfm (sign sk s $
    addTags "Егор Летов" ["russian", "black metal"] <> apiKey ak <> json)

  testGetTagsAuth = assert $ parse gt <:> lastfm (sign sk s $
    getTags <> artist "Егор Летов" <> apiKey ak <> json)

  testRemoveTag = assert $ parse ok <:> lastfm (sign sk s $
    removeTag "Егор Летов" "russian" <> apiKey ak <> json)

  testShare = assert $ parse ok <:> lastfm (sign sk s $
    share "Sleep" "liblastfm" <> message "Just listen!" <> apiKey ak <> json)


noauth ∷ [Test]
noauth =
  [ TestLabel "Artist.getCorrection" $ TestCase testGetCorrection
  , TestLabel "Artist.getEvents" $ TestCase testGetEvents
  , TestLabel "Artist.getInfo" $ TestCase testGetInfo
  , TestLabel "Artist.getPastEvents" $ TestCase testGetPastEvents
  , TestLabel "Artist.getPodcast" $ TestCase testGetPodcast
  , TestLabel "Artist.getShouts" $ TestCase testGetShouts
  , TestLabel "Artist.getSimilar" $ TestCase testGetSimilar
  , TestLabel "Artist.getTags" $ TestCase testGetTags
  , TestLabel "Artist.getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "Artist.getTopFans" $ TestCase testGetTopFans
  , TestLabel "Artist.getTopTags" $ TestCase testGetTopTags
  , TestLabel "Artist.getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "Artist.search" $ TestCase testSearch
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetCorrection = assert $ parse gc <:> lastfm (
    getCorrection "Meshugah" <> apiKey ak <> json)

  testGetEvents = assert $ parse ge <:> lastfm (
    getEvents <> artist "Meshuggah" <> limit 2 <> apiKey ak <> json)

  testGetInfo = assert $ parse gin <:> lastfm (
    getInfo <> artist "Meshuggah" <> apiKey ak <> json)

  testGetPastEvents = assert $ parse gpe <:> lastfm (
    getPastEvents <> artist "Meshuggah" <> autocorrect True <> apiKey ak <> json)

  testGetPodcast = assert $ parse gp <:> lastfm (
    getPodcast <> artist "Meshuggah" <> apiKey ak <> json)

  testGetShouts = assert $ parse gs <:> lastfm (
    getShouts <> artist "Meshuggah" <> limit 5 <> apiKey ak <> json)

  testGetSimilar = assert $ parse gsi <:> lastfm (
    getSimilar <> artist "Meshuggah" <> limit 3 <> apiKey ak <> json)

  testGetTags = assert $ parse gt <:> lastfm (
    getTags <> artist "Егор Летов" <> user "liblastfm" <> apiKey ak <> json)

  testGetTopAlbums = assert $ parse gta <:> lastfm (
    getTopAlbums <> artist "Meshuggah" <> limit 3 <> apiKey ak <> json)

  testGetTopFans = assert $ parse gtf <:> lastfm (
    getTopFans <> artist "Meshuggah" <> apiKey ak <> json)

  testGetTopTags = assert $ parse gtt <:> lastfm (
    getTopTags <> artist "Meshuggah" <> apiKey ak <> json)

  testGetTopTracks = assert $ parse gttr <:> lastfm (
    getTopTracks <> artist "Meshuggah" <> limit 3 <> apiKey ak <> json)

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
