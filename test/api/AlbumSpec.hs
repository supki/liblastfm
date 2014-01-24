{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module AlbumSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Album
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Album.addTags" $
    query_ . sign privateSecret $
      addTags <*> artist "Pink Floyd" <*> album "The Wall" <*> tags ["70s", "awesome", "classic"]
        <*> privateAPIKey <*> privateSessionKey

  it "Album.getTags-authenticated" $
    query gt . sign privateSecret $
      getTags <*> artist "Pink Floyd" <*> album "The Wall"
        <*> privateAPIKey <* privateSessionKey

  it "Album.removeTag" $
    query_ . sign privateSecret $
      removeTag <*> artist "Pink Floyd" <*> album "The Wall" <*> tag "awesome"
        <*> privateAPIKey <*> privateSessionKey

  it "Album.share" $
    query_ . sign privateSecret $
      share <*> album "Jerusalem" <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"
        <*> privateAPIKey <*> privateSessionKey

  it "Album.getBuyLinks" $
    query gbl $
      getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> album "The Wall" <*> publicKey

  it "Album.getBuyLinks_mbid" $
    query gbl $
      getBuyLinks <*> country "United Kingdom" <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <*> publicKey

  it "Album.getInfo" $
    query gi $
      getInfo <*> artist "Pink Floyd" <*> album "The Wall" <*> publicKey

  it "Album.getInfo_mbid" $
    query gi $
      getInfo <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <*> publicKey

  it "Album.getShouts" $
    query gs $
      getShouts <*> artist "Pink Floyd" <*> album "The Wall" <* limit 7 <*> publicKey

  it "Album.getShouts_mbid" $
    query gs $
      getShouts <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* limit 7 <*> publicKey

  it "Album.getTags" $
    query gt $
      getTags <*> artist "Pink Floyd" <*> album "The Wall" <* user "liblastfm" <*> publicKey

  it "Album.getTags_mbid" $
    query gt $
      getTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <* user "liblastfm" <*> publicKey

  it "Album.getTopTags" $
    query gtt $
      getTopTags <*> artist "Pink Floyd" <*> album "The Wall" <*> publicKey

  it "Album.getTopTags_mbid" $
    query gtt $
      getTopTags <*> mbid "816abcd3-924a-3565-92b9-7ab750688f34" <*> publicKey

  it "Album.search" $
    query se $
      search <*> album "wall" <* limit 5 <*> publicKey

gbl, gi, gs, gt, gtt, se :: Query Text
gbl = key "affiliations".key "physicals".key "affiliation".values.key "supplierName"._String
gi  = key "album".key "toptags".key "tag".values.key "name"._String
gs  = key "shouts".key "shout".values.key "body"._String
gt  = key "tags".key "tag".values.key "name"._String
gtt = key "toptags".key "tag".values.key "count"._String
se  = key "results".key "albummatches".key "album".values.key "name"._String
