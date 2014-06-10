{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.TrackSpec (spec) where

import Data.Text (Text)
import Data.Traversable (traverse)
import Network.Lastfm
import Network.Lastfm.Track
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "addTags" $
    shouldHaveXml_ . privately $
      addTags <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tags ["60s", "awesome"]

  it "ban" $
    shouldHaveXml_ . privately $
      ban <*> artist "Eminem" <*> track "Kim"

  it "love" $
    shouldHaveXml_ . privately $
      love <*> artist "Gojira" <*> track "Ocean"

  it "removeTag" $
    shouldHaveXml_ . privately $
      removeTag <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tag "awesome"

  it "share" $
    shouldHaveXml_ . privately $
      share <*> artist "Led Zeppelin" <*> track "When the Levee Breaks" <*> recipient "liblastfm" <* message "Just listen!"

  it "unban" $
    shouldHaveXml_ . privately $
      unban <*> artist "Eminem" <*> track "Kim"

  it "unlove" $
    shouldHaveXml_ . privately $
      unlove <*> artist "Gojira" <*> track "Ocean"

  it "scrobble" $
    privately (scrobble (pure (item <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1300000000)))
   `shouldHaveXml`
    root.node "scrobbles".node "scrobble".node "track".text

  it "updateNowPlaying" $
    privately (updateNowPlaying <*> artist "Gojira" <*> track "Ocean")
   `shouldHaveXml`
    root.node "nowplaying".node "track".text

  it "getBuylinks" $
    publicly (getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> track "Brain Damage")
   `shouldHaveXml`
    root.node "affiliations".node "downloads".node "affiliation".node "supplierName".text

  it "getCorrection" $
    publicly (getCorrection <*> artist "Pink Ployd" <*> track "Brain Damage")
   `shouldHaveXml`
    root.node "corrections".node "correction".node "track".node "artist".node "name".text

  it "getFingerprintMetadata" $
    publicly (getFingerprintMetadata <*> fingerprint 1234)
   `shouldHaveXml`
    root.node "tracks".node "track".node "name".text

  describe "getInfo*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "track".node "userplaycount".text

    it "getInfo" $
      publicly (getInfo <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* username "aswalrus")
     `shouldHaveXml`
      xmlQuery

    it "getInfo_mbid" $
      publicly (getInfo <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* username "aswalrus")
     `shouldHaveXml`
      xmlQuery

  describe "getShouts*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "shouts".node "shout".node "author".text

    it "getShouts" $
      publicly (getShouts <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 7)
     `shouldHaveXml`
      xmlQuery

    it "getShouts_mbid" $
      publicly (getShouts <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 7)
     `shouldHaveXml`
      xmlQuery

  describe "getSimilar*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "similartracks".node "track".node "name".text

    it "getSimilar" $
      publicly (getSimilar <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 4)
     `shouldHaveXml`
      xmlQuery

    it "getSimilar_mbid" $
      publicly (getSimilar <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 4)
     `shouldHaveXml`
      xmlQuery

  describe "getTags*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "tags".attr "track".traverse

    it "getTags" $
      publicly (getTags <*> artist "Jefferson Airplane" <*> track "White Rabbit" <* user "liblastfm")
     `shouldHaveXml`
      xmlQuery

    it "getTags_mbid" $
      publicly (getTags <*> mbid "001b3337-faf4-421a-a11f-45e0b60a7703"  <* user "liblastfm")
     `shouldHaveXml`
      xmlQuery

  describe "getTopFans*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "topfans".node "user".node "name".text

    it "getTopFans" $
      publicly (getTopFans <*> artist "Pink Floyd" <*> track "Comfortably Numb")
     `shouldHaveXml`
      xmlQuery

    it "getTopFans_mbid" $
      publicly (getTopFans <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92")
     `shouldHaveXml`
      xmlQuery

  describe "getTopTags*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "toptags".node "tag".node "name".text

    it "getTopTags" $
      publicly (getTopTags <*> artist "Pink Floyd" <*> track "Comfortably Numb")
     `shouldHaveXml`
      xmlQuery

    it "getTopTags_mbid" $
      publicly (getTopTags <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92")
     `shouldHaveXml`
      xmlQuery

  it "search" $
    publicly (search <*> track "Believe" <* limit 12)
   `shouldHaveXml`
    root.node "results".node "trackmatches".node "track".node "name".text
