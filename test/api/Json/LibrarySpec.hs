{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.LibrarySpec (spec) where

import Data.Aeson.Lens
import Network.Lastfm
import Network.Lastfm.Library
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "addAlbum" $
    shouldHaveJson_ . privately $
      addAlbum (pure (albumItem <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"))

  it "addArtist" $
    shouldHaveJson_ . privately $
      addArtist (pure (artistItem <*> artist "Mobthrow"))

  it "addTrack" $
    shouldHaveJson_ . privately $
      addTrack <*> artist "Eminem" <*> track "Kim"

  it "removeAlbum" $
    shouldHaveJson_ . privately $
      removeAlbum <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"

  it "removeArtist" $
    shouldHaveJson_ . privately $
      removeArtist <*> artist "Burzum"

  it "removeTrack" $
    shouldHaveJson_ . privately $
      removeTrack <*> artist "Eminem" <*> track "Kim"

  it "removeScrobble" $
    shouldHaveJson_ . privately $
      removeScrobble <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1328905590

  it "getAlbums" $
    publicly (getAlbums <*> user "smpcln" <* artist "Burzum" <* limit 5)
   `shouldHaveJson`
    key "albums".key "album".values.key "name"._String

  it "getArtists" $
    publicly (getArtists <*> user "smpcln" <* limit 7)
   `shouldHaveJson`
    key "artists".key "artist".values.key "name"._String

  it "getTracks" $
    publicly (getTracks <*> user "smpcln" <* artist "Burzum" <* limit 4)
   `shouldHaveJson`
    key "tracks".key "track".values.key "name"._String
