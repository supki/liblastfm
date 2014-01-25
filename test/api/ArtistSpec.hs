{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module ArtistSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Artist
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "addTags" $
    shouldHaveJson_ . privately $
      addTags <*> artist "Егор Летов" <*> tags ["russian", "black metal"]

  it "getTags-authenticated" $
    privately (getTags <*> artist "Егор Летов")
   `shouldHaveJson`
    key "tags".key "tag".values.key "name"._String

  it "removeTag" $
    shouldHaveJson_ . privately $
      removeTag <*> artist "Егор Летов" <*> tag "russian"

  it "share" $
    shouldHaveJson_ . privately $
      share <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"

  it "getCorrection" $
    publicly (getCorrection <*> artist "Meshugah")
   `shouldHaveJson`
    key "corrections".key "correction".key "artist".key "name"._String

  describe "getEvents*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "events".key "artist"._String

    it "getEvents" $
      publicly (getEvents <*> artist "Meshuggah" <* limit 2)
     `shouldHaveJson`
      jsonQuery

    it "getEvents_mbid" $
      publicly (getEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 2)
     `shouldHaveJson`
      jsonQuery

  describe "getInfo*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "artist".key "stats".key "listeners"._String

    it "getInfo" $
      publicly (getInfo <*> artist "Meshuggah")
     `shouldHaveJson`
      jsonQuery

    it "getInfo_mbid" $
      publicly (getInfo <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveJson`
      jsonQuery

  describe "getPastEvents*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "events".key "event".values.key "title"._String

    it "getPastEvents" $
      publicly (getPastEvents <*> artist "Meshuggah" <* autocorrect True)
     `shouldHaveJson`
      jsonQuery

    it "getPastEvents_mbid" $
      publicly (getPastEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* autocorrect True)
     `shouldHaveJson`
      jsonQuery

  describe "getPodcast*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "rss".key "channel".key "description"._String

    it "getPodcast" $
      publicly (getPodcast <*> artist "Meshuggah")
     `shouldHaveJson`
      jsonQuery

    it "getPodcast_mbid" $
      publicly (getPodcast <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveJson`
      jsonQuery

  describe "getShouts*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "shouts".key "shout".values.key "author"._String

    it "getShouts" $
      publicly (getShouts <*> artist "Meshuggah" <* limit 5)
     `shouldHaveJson`
      jsonQuery

    it "getShouts_mbid" $
      publicly (getShouts <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 5)
     `shouldHaveJson`
      jsonQuery

  describe "getSimilar*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "similarartists".key "artist".values.key "name"._String

    it "getSimilar" $
      publicly (getSimilar <*> artist "Meshuggah" <* limit 3)
     `shouldHaveJson`
      jsonQuery

    it "getSimilar_mbid" $
      publicly (getSimilar <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3)
     `shouldHaveJson`
      jsonQuery

  describe "getTags*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "tags".key "tag".values.key "name"._String

    it "getTags" $
      publicly (getTags <*> artist "Егор Летов" <* user "liblastfm")
     `shouldHaveJson`
      jsonQuery

    it "getTags_mbid" $
      publicly (getTags <*> mbid "cfb3d32e-d095-4d63-946d-9daf06932180" <* user "liblastfm")
     `shouldHaveJson`
      jsonQuery

  describe "getTopAlbums*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "topalbums".key "album".values.key "name"._String

    it "getTopAlbums" $
      publicly (getTopAlbums <*> artist "Meshuggah" <* limit 3)
     `shouldHaveJson`
      jsonQuery

    it "getTopAlbums_mbid" $
      publicly (getTopAlbums <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3)
     `shouldHaveJson`
      jsonQuery

  describe "getTopFans*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "topfans".key "user".values.key "name"._String

    it "getTopFans" $
      publicly (getTopFans <*> artist "Meshuggah")
     `shouldHaveJson`
      jsonQuery

    it "getTopFans_mbid" $
      publicly (getTopFans <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveJson`
      jsonQuery

  describe "getTopTags*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "toptags".key "tag".values.key "name"._String

    it "getTopTags" $
      publicly (getTopTags <*> artist "Meshuggah")
     `shouldHaveJson`
      jsonQuery

    it "getTopTags_mbid" $
      publicly (getTopTags <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveJson`
      jsonQuery

  describe "getTopTracks*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "toptracks".key "track".values.key "name"._String

    it "getTopTracks" $
      publicly (getTopTracks <*> artist "Meshuggah" <* limit 3)
     `shouldHaveJson`
      jsonQuery

    it "getTopTracks_mbid" $
      publicly (getTopTracks <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3)
     `shouldHaveJson`
      jsonQuery

  it "search" $
    publicly (search <*> artist "Mesh" <* limit 3)
   `shouldHaveJson`
    key "results".key "artistmatches".key "artist".values.key "name"._String
