{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Library (auth, noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Library
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Library.addAlbum" testAddAlbum
  , testCase "Library.addArtist" testAddArtist
  , testCase "Library.addTrack" testAddTrack
  , testCase "Library.removeAlbum" testRemoveAlbum
  , testCase "Library.removeArtist" testRemoveArtist
  , testCase "Library.removeTrack" testRemoveTrack
  , testCase "Library.removeScrobble" testRemoveScrobble
  ]
 where
  testAddAlbum = query ok . sign s $
    addAlbum (pure (albumItem <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand")) <*> ak <*> sk

  testAddArtist = query ok . sign s $
    addArtist (pure (artistItem <*> artist "Mobthrow")) <*> ak <*> sk

  testAddTrack = query ok . sign s $
    addTrack <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testRemoveAlbum = query ok . sign s $
    removeAlbum <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand" <*> ak <*> sk

  testRemoveArtist = query ok . sign s $
    removeArtist <*> artist "Burzum" <*> ak <*> sk

  testRemoveTrack = query ok . sign s $
    removeTrack <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testRemoveScrobble = query ok . sign s $
    removeScrobble <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1328905590 <*> ak <*> sk


noauth :: Request JSON APIKey -> [Test]
noauth ak =
  [ testCase "Library.getAlbums" testGetAlbums
  , testCase "Library.getArtists" testGetArtists
  , testCase "Library.getTracks" testGetTracks
  ]
 where
  testGetAlbums = query ga $
    getAlbums <*> user "smpcln" <* artist "Burzum" <* limit 5 <*> ak

  testGetArtists = query gar $
    getArtists <*> user "smpcln" <* limit 7 <*> ak

  testGetTracks = query gt $
    getTracks <*> user "smpcln" <* artist "Burzum" <* limit 4 <*> ak


ga, gar, gt :: Query Text
ga  = key "albums".key "album".values.key "name"._String
gar = key "artists".key "artist".values.key "name"._String
gt  = key "tracks".key "track".values.key "name"._String
