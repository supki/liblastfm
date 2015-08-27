{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.AlbumSpec (spec) where

import Data.Text (Text)
import Lastfm
import Lastfm.Album
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "addTags" $
    shouldHaveXml_ . privately $
      addTags <*> artist "Pink Floyd" <*> album "The Wall" <*> tags ["70s", "awesome", "classic"]

  it "getTags-authenticated" $
    privately (getTags <*> artist "Pink Floyd" <*> album "The Wall")
   `shouldHaveXml`
    root.node "tags".node "tag".node "name".text

  it "removeTag" $
    shouldHaveXml_ .  privately $
      removeTag <*> artist "Pink Floyd" <*> album "The Wall" <*> tag "awesome"

  it "share" $
    shouldHaveXml_ .  privately $
      share <*> album "Jerusalem" <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"

  describe "getBuyLinks*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "affiliations".node "physicals".node "affiliation".node "supplierName".text

    it "getBuyLinks" $
      publicly (getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> album "The Wall")
     `shouldHaveXml`
      xmlQuery

    it "getBuyLinks_mbid" $
      publicly (getBuyLinks <*> country "United Kingdom" <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34")
     `shouldHaveXml`
      xmlQuery

  describe "getInfo*" $ do
    let jsonQuery :: Fold Document Text
        jsonQuery = root.node "album".node "toptags".node "tag".node "name".text

    it "getInfo" $
      publicly (getInfo <*> artist "Pink Floyd" <*> album "The Wall")
     `shouldHaveXml`
      jsonQuery

    it "getInfo_mbid" $
      publicly (getInfo <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34")
     `shouldHaveXml`
      jsonQuery

  describe "getShouts*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "shouts".node "shout".node "body".text

    it "getShouts" $
      publicly (getShouts <*> artist "Pink Floyd" <*> album "The Wall" <* limit 7)
     `shouldHaveXml`
      xmlQuery

    it "getShouts_mbid" $
      publicly (getShouts <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* limit 7)
     `shouldHaveXml`
      xmlQuery

  describe "getTags*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "tags".node "tag".node "name".text

    it "getTags" $
      publicly (getTags <*> artist "Pink Floyd" <*> album "The Wall" <* user "liblastfm")
     `shouldHaveXml`
      xmlQuery

    it "getTags_mbid" $
      publicly (getTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* user "liblastfm")
     `shouldHaveXml`
      xmlQuery

  describe "getTopTags*" $ do
    let xmlQuery :: Fold Document Text
        xmlQuery = root.node "toptags".node "tag".node "count".text

    it "getTopTags" $
      publicly (getTopTags <*> artist "Pink Floyd" <*> album "The Wall")
     `shouldHaveXml`
      xmlQuery

    it "getTopTags_mbid" $
      publicly (getTopTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34")
     `shouldHaveXml`
      xmlQuery

  it "search" $
    publicly (search <*> album "wall" <* limit 5)
   `shouldHaveXml`
    root.node "results".node "albummatches".node "album".node "name".text
