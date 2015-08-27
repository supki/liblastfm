{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.AlbumSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Text (Text)
import Lastfm
import Lastfm.Album
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "addTags" $
    shouldHaveJson_ . privately $
      addTags <*> artist "Pink Floyd" <*> album "The Wall" <*> tags ["70s", "awesome", "classic"]

  it "getTags-authenticated" $
    privately (getTags <*> artist "Pink Floyd" <*> album "The Wall")
   `shouldHaveJson`
    key "tags".key "tag".values.key "name"._String

  it "removeTag" $
    shouldHaveJson_ .  privately $
      removeTag <*> artist "Pink Floyd" <*> album "The Wall" <*> tag "awesome"

  it "share" $
    shouldHaveJson_ .  privately $
      share <*> album "Jerusalem" <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"

  describe "getBuyLinks*" $ do
    let jsonQuery :: Fold Value Text
        jsonQuery = key "affiliations".key "physicals".key "affiliation".values.key "supplierName"._String

    it "getBuyLinks" $
      publicly (getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> album "The Wall")
     `shouldHaveJson`
      jsonQuery

    it "getBuyLinks_mbid" $
      publicly (getBuyLinks <*> country "United Kingdom" <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34")
     `shouldHaveJson`
      jsonQuery

  describe "getInfo*" $ do
    let jsonQuery :: Fold Value Text
        jsonQuery = key "album".key "toptags".key "tag".values.key "name"._String

    it "getInfo" $
      publicly (getInfo <*> artist "Pink Floyd" <*> album "The Wall")
     `shouldHaveJson`
      jsonQuery

    it "getInfo_mbid" $
      publicly (getInfo <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34")
     `shouldHaveJson`
      jsonQuery

  describe "getShouts*" $ do
    let jsonQuery :: Fold Value Text
        jsonQuery = key "shouts".key "shout".values.key "body"._String

    it "getShouts" $
      publicly (getShouts <*> artist "Pink Floyd" <*> album "The Wall" <* limit 7)
     `shouldHaveJson`
      jsonQuery

    it "getShouts_mbid" $
      publicly (getShouts <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* limit 7)
     `shouldHaveJson`
      jsonQuery

  describe "getTags*" $ do
    let jsonQuery :: Fold Value Text
        jsonQuery = key "tags".key "tag".values.key "name"._String

    it "getTags" $
      publicly (getTags <*> artist "Pink Floyd" <*> album "The Wall" <* user "liblastfm")
     `shouldHaveJson`
      jsonQuery

    it "getTags_mbid" $
      publicly (getTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* user "liblastfm")
     `shouldHaveJson`
      jsonQuery

  describe "getTopTags*" $ do
    let jsonQuery :: Fold Value Text
        jsonQuery = key "toptags".key "tag".values.key "count"._String

    it "getTopTags" $
      publicly (getTopTags <*> artist "Pink Floyd" <*> album "The Wall")
     `shouldHaveJson`
      jsonQuery

    it "getTopTags_mbid" $
      publicly (getTopTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34")
     `shouldHaveJson`
      jsonQuery

  it "search" $
    publicly (search <*> album "wall" <* limit 5)
   `shouldHaveJson`
    key "results".key "albummatches".key "album".values.key "name"._String
