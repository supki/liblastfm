{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.TrackSpec (spec) where

import Data.Aeson.Lens
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Track
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "addTags" $
    shouldHaveJson_ . privately $
      addTags <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tags ["60s", "awesome"]

  it "ban" $
    shouldHaveJson_ . privately $
      ban <*> artist "Eminem" <*> track "Kim"

  it "love" $
    shouldHaveJson_ . privately $
      love <*> artist "Gojira" <*> track "Ocean"

  it "removeTag" $
    shouldHaveJson_ . privately $
      removeTag <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tag "awesome"

  it "share" $
    shouldHaveJson_ . privately $
      share <*> artist "Led Zeppelin" <*> track "When the Levee Breaks" <*> recipient "liblastfm" <* message "Just listen!"

  it "unban" $
    shouldHaveJson_ . privately $
      unban <*> artist "Eminem" <*> track "Kim"

  it "unlove" $
    shouldHaveJson_ . privately $
      unlove <*> artist "Gojira" <*> track "Ocean"

  it "scrobble" $
    privately (scrobble (pure (item <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1300000000)))
   `shouldHaveJson`
    key "scrobbles".key "scrobble".key "track".key "#text"._String

  it "updateNowPlaying" $
    privately (updateNowPlaying <*> artist "Gojira" <*> track "Ocean")
   `shouldHaveJson`
    key "nowplaying".key "track".key "#text"._String

  it "getBuylinks" $
    publicly (getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> track "Brain Damage")
   `shouldHaveJson`
    key "affiliations".key "downloads".key "affiliation".values.key "supplierName"._String

  it "getCorrection" $
    publicly (getCorrection <*> artist "Pink Ployd" <*> track "Brain Damage")
   `shouldHaveJson`
    key "corrections".key "correction".key "track".key "artist".key "name"._String

  it "getFingerprintMetadata" $
    publicly (getFingerprintMetadata <*> fingerprint 1234)
   `shouldHaveJson`
    key "tracks".key "track".values.key "name"._String

  describe "getInfo*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "track".key "userplaycount"._String

    it "getInfo" $
      publicly (getInfo <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* username "aswalrus")
     `shouldHaveJson`
      jsonQuery

    it "getInfo_mbid" $
      publicly (getInfo <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* username "aswalrus")
     `shouldHaveJson`
      jsonQuery

  describe "getShouts*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "shouts".key "shout".values.key "author"._String

    it "getShouts" $
      publicly (getShouts <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 7)
     `shouldHaveJson`
      jsonQuery

    it "getShouts_mbid" $
      publicly (getShouts <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 7)
     `shouldHaveJson`
      jsonQuery

  describe "getSimilar*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "similartracks".key "track".values.key "name"._String

    it "getSimilar" $
      publicly (getSimilar <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 4)
     `shouldHaveJson`
      jsonQuery

    it "getSimilar_mbid" $
      publicly (getSimilar <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 4)
     `shouldHaveJson`
      jsonQuery

  describe "getTags*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "tags".key "@attr".key "track"._String

    it "getTags" $
      publicly (getTags <*> artist "Jefferson Airplane" <*> track "White Rabbit" <* user "liblastfm")
     `shouldHaveJson`
      jsonQuery

    it "getTags_mbid" $
      publicly (getTags <*> mbid "001b3337-faf4-421a-a11f-45e0b60a7703"  <* user "liblastfm")
     `shouldHaveJson`
      jsonQuery

  describe "getTopFans*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "topfans".key "user".values.key "name"._String

    it "getTopFans" $
      publicly (getTopFans <*> artist "Pink Floyd" <*> track "Comfortably Numb")
     `shouldHaveJson`
      jsonQuery

    it "getTopFans_mbid" $
      publicly (getTopFans <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92")
     `shouldHaveJson`
      jsonQuery

  describe "getTopTags*" $ do
    let jsonQuery :: Query JSON Text
        jsonQuery = key "toptags".key "tag".values.key "name"._String

    it "getTopTags" $
      publicly (getTopTags <*> artist "Pink Floyd" <*> track "Comfortably Numb")
     `shouldHaveJson`
      jsonQuery

    it "getTopTags_mbid" $
      publicly (getTopTags <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92")
     `shouldHaveJson`
      jsonQuery

  it "search" $
    publicly (search <*> track "Believe" <* limit 12)
   `shouldHaveJson`
    key "results".key "trackmatches".key "track".values.key "name"._String
