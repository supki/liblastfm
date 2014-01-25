{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.ArtistSpec (spec) where

import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Artist
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "addTags" $
    shouldHaveXml_ . privately $
      addTags <*> artist "Егор Летов" <*> tags ["russian", "black metal"]

  it "getTags-authenticated" $
    privately (getTags <*> artist "Егор Летов")
   `shouldHaveXml`
    root.node "tags".node "tag".node "name".text

  it "removeTag" $
    shouldHaveXml_ . privately $
      removeTag <*> artist "Егор Летов" <*> tag "russian"

  it "share" $
    shouldHaveXml_ . privately $
      share <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"

  it "getCorrection" $
    publicly (getCorrection <*> artist "Meshugah")
   `shouldHaveXml`
    root.node "corrections".node "correction".node "artist".node "name".text

  describe "getEvents*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "events".attr "artist"

    it "getEvents" $
      publicly (getEvents <*> artist "Meshuggah" <* limit 2)
     `shouldHaveXml`
      xmlQuery

    it "getEvents_mbid" $
      publicly (getEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 2)
     `shouldHaveXml`
      xmlQuery

  describe "getInfo*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "artist".node "stats".node "listeners".text

    it "getInfo" $
      publicly (getInfo <*> artist "Meshuggah")
     `shouldHaveXml`
      xmlQuery

    it "getInfo_mbid" $
      publicly (getInfo <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveXml`
      xmlQuery

  describe "getPastEvents*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "events".node "event".node "title".text

    it "getPastEvents" $
      publicly (getPastEvents <*> artist "Meshuggah" <* autocorrect True)
     `shouldHaveXml`
      xmlQuery

    it "getPastEvents_mbid" $
      publicly (getPastEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* autocorrect True)
     `shouldHaveXml`
      xmlQuery

  describe "getPodcast*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "rss".node "channel".node "description".text

    it "getPodcast" $
      publicly (getPodcast <*> artist "Meshuggah")
     `shouldHaveXml`
      xmlQuery

    it "getPodcast_mbid" $
      publicly (getPodcast <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveXml`
      xmlQuery

  describe "getShouts*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "shouts".node "shout".node "author".text

    it "getShouts" $
      publicly (getShouts <*> artist "Meshuggah" <* limit 5)
     `shouldHaveXml`
      xmlQuery

    it "getShouts_mbid" $
      publicly (getShouts <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 5)
     `shouldHaveXml`
      xmlQuery

  describe "getSimilar*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "similarartists".node "artist".node "name".text

    it "getSimilar" $
      publicly (getSimilar <*> artist "Meshuggah" <* limit 3)
     `shouldHaveXml`
      xmlQuery

    it "getSimilar_mbid" $
      publicly (getSimilar <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3)
     `shouldHaveXml`
      xmlQuery

  describe "getTags*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "tags".node "tag".node "name".text

    it "getTags" $
      publicly (getTags <*> artist "Егор Летов" <* user "liblastfm")
     `shouldHaveXml`
      xmlQuery

    it "getTags_mbid" $
      publicly (getTags <*> mbid "cfb3d32e-d095-4d63-946d-9daf06932180" <* user "liblastfm")
     `shouldHaveXml`
      xmlQuery

  describe "getTopAlbums*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "topalbums".node "album".node "name".text

    it "getTopAlbums" $
      publicly (getTopAlbums <*> artist "Meshuggah" <* limit 3)
     `shouldHaveXml`
      xmlQuery

    it "getTopAlbums_mbid" $
      publicly (getTopAlbums <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3)
     `shouldHaveXml`
      xmlQuery

  describe "getTopFans*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "topfans".node "user".node "name".text

    it "getTopFans" $
      publicly (getTopFans <*> artist "Meshuggah")
     `shouldHaveXml`
      xmlQuery

    it "getTopFans_mbid" $
      publicly (getTopFans <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveXml`
      xmlQuery

  describe "getTopTags*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "toptags".node "tag".node "name".text

    it "getTopTags" $
      publicly (getTopTags <*> artist "Meshuggah")
     `shouldHaveXml`
      xmlQuery

    it "getTopTags_mbid" $
      publicly (getTopTags <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413")
     `shouldHaveXml`
      xmlQuery

  describe "getTopTracks*" $ do
    let xmlQuery :: Query XML Text
        xmlQuery = root.node "toptracks".node "track".node "name".text

    it "getTopTracks" $
      publicly (getTopTracks <*> artist "Meshuggah" <* limit 3)
     `shouldHaveXml`
      xmlQuery

    it "getTopTracks_mbid" $
      publicly (getTopTracks <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3)
     `shouldHaveXml`
      xmlQuery

  it "search" $
    publicly (search <*> artist "Mesh" <* limit 3)
   `shouldHaveXml`
    root.node "results".node "artistmatches".node "artist".node "name".text
