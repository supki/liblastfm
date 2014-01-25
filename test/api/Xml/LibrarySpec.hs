{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.LibrarySpec (spec) where

import Network.Lastfm
import Network.Lastfm.Library
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "addAlbum" $
    shouldHaveXml_ . privately $
      addAlbum (pure (albumItem <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"))

  it "addArtist" $
    shouldHaveXml_ . privately $
      addArtist (pure (artistItem <*> artist "Mobthrow"))

  it "addTrack" $
    shouldHaveXml_ . privately $
      addTrack <*> artist "Eminem" <*> track "Kim"

  it "removeAlbum" $
    shouldHaveXml_ . privately $
      removeAlbum <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"

  it "removeArtist" $
    shouldHaveXml_ . privately $
      removeArtist <*> artist "Burzum"

  it "removeTrack" $
    shouldHaveXml_ . privately $
      removeTrack <*> artist "Eminem" <*> track "Kim"

  it "removeScrobble" $
    shouldHaveXml_ . privately $
      removeScrobble <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1328905590

  it "getAlbums" $
    publicly (getAlbums <*> user "smpcln" <* artist "Burzum" <* limit 5)
   `shouldHaveXml`
    root.node "albums".node "album".node "name".text

  it "getArtists" $
    publicly (getArtists <*> user "smpcln" <* limit 7)
   `shouldHaveXml`
    root.node "artists".node "artist".node "name".text

  it "getTracks" $
    publicly (getTracks <*> user "smpcln" <* artist "Burzum" <* limit 4)
   `shouldHaveXml`
    root.node "tracks".node "track".node "name".text
