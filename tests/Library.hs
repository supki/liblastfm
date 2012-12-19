{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Library (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Library
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
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
    addAlbum <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"
      <*> apiKey ak <*> sessionKey sk

  testAddArtist = check ok . sign s $
    addArtist <*> artist "Mobthrow"
      <*> apiKey ak <*> sessionKey sk

  testAddTrack = check ok . sign s $
    addTrack <*> artist "Eminem" <*> track "Kim"
      <*> apiKey ak <*> sessionKey sk

  testRemoveAlbum = check ok . sign s $
    removeAlbum <*> artist "Franz Ferdinand" <*> album "Franz Ferdinand"
      <*> apiKey ak <*> sessionKey sk

  testRemoveArtist = check ok . sign s $
    removeArtist <*> artist "Burzum"
      <*> apiKey ak <*> sessionKey sk

  testRemoveTrack = check ok . sign s $
    removeTrack <*> artist "Eminem" <*> track "Kim"
      <*> apiKey ak <*> sessionKey sk

  testRemoveScrobble = check ok . sign s $
    removeScrobble <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1328905590
      <*> apiKey ak <*> sessionKey sk


noauth ∷ [Test]
noauth =
  [ testCase "Library.getAlbums" testGetAlbums
  , testCase "Library.getArtists" testGetArtists
  , testCase "Library.getTracks" testGetTracks
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetAlbums = check ga $
    getAlbums <*> user "smpcln" <* artist "Burzum" <* limit 5 <*> apiKey ak

  testGetArtists = check gar $
    getArtists <*> user "smpcln" <* limit 7 <*> apiKey ak

  testGetTracks = check gt $
    getTracks <*> user "smpcln" <* artist "Burzum" <* limit 4 <*> apiKey ak


ga, gar, gt ∷ Value → Parser [String]
ga o = parseJSON o >>= (.: "albums") >>= (.: "album") >>= mapM (.: "name")
gar o = parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
