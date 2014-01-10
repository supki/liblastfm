{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Library (auth, noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Library
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


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
  testAddAlbum = check ok . sign s $
    addAlbum (pure (albumItem <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand")) <*> ak <*> sk

  testAddArtist = check ok . sign s $
    addArtist (pure (artistItem <*> artist "Mobthrow")) <*> ak <*> sk

  testAddTrack = check ok . sign s $
    addTrack <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testRemoveAlbum = check ok . sign s $
    removeAlbum <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand" <*> ak <*> sk

  testRemoveArtist = check ok . sign s $
    removeArtist <*> artist "Burzum" <*> ak <*> sk

  testRemoveTrack = check ok . sign s $
    removeTrack <*> artist "Eminem" <*> track "Kim" <*> ak <*> sk

  testRemoveScrobble = check ok . sign s $
    removeScrobble <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1328905590 <*> ak <*> sk


noauth :: Request JSON APIKey -> [Test]
noauth ak =
  [ testCase "Library.getAlbums" testGetAlbums
  , testCase "Library.getArtists" testGetArtists
  , testCase "Library.getTracks" testGetTracks
  ]
 where
  testGetAlbums = check ga $
    getAlbums <*> user "smpcln" <* artist "Burzum" <* limit 5 <*> ak

  testGetArtists = check gar $
    getArtists <*> user "smpcln" <* limit 7 <*> ak

  testGetTracks = check gt $
    getTracks <*> user "smpcln" <* artist "Burzum" <* limit 4 <*> ak


ga, gar, gt :: Value -> Parser [String]
ga o = parseJSON o >>= (.: "albums") >>= (.: "album") >>= mapM (.: "name")
gar o = parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
