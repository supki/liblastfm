{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module LibrarySpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Library
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Library.addAlbum" $
    query_ . sign privateSecret $
      addAlbum (pure (albumItem <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"))
      <*> privateAPIKey <*> privateSessionKey

  it "Library.addArtist" $
    query_ . sign privateSecret $
      addArtist (pure (artistItem <*> artist "Mobthrow"))
      <*> privateAPIKey <*> privateSessionKey

  it "Library.addTrack" $
    query_ . sign privateSecret $
      addTrack <*> artist "Eminem" <*> track "Kim"
      <*> privateAPIKey <*> privateSessionKey

  it "Library.removeAlbum" $
    query_ . sign privateSecret $
      removeAlbum <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"
      <*> privateAPIKey <*> privateSessionKey

  it "Library.removeArtist" $
    query_ . sign privateSecret $
      removeArtist <*> artist "Burzum"
      <*> privateAPIKey <*> privateSessionKey

  it "Library.removeTrack" $
    query_ . sign privateSecret $
      removeTrack <*> artist "Eminem" <*> track "Kim"
      <*> privateAPIKey <*> privateSessionKey

  it "Library.removeScrobble" $
    query_ . sign privateSecret $
      removeScrobble <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1328905590
      <*> privateAPIKey <*> privateSessionKey

  it "Library.getAlbums" $
    query ga $
      getAlbums <*> user "smpcln" <* artist "Burzum" <* limit 5 <*> publicKey

  it "Library.getArtists" $
    query gar $
      getArtists <*> user "smpcln" <* limit 7 <*> publicKey

  it "Library.getTracks" $
    query gt $
      getTracks <*> user "smpcln" <* artist "Burzum" <* limit 4 <*> publicKey

ga, gar, gt :: Query Text
ga  = key "albums".key "album".values.key "name"._String
gar = key "artists".key "artist".values.key "name"._String
gt  = key "tracks".key "track".values.key "name"._String
