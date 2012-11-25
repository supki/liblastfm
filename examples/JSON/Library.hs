{-# LANGUAGE FlexibleInstances #-}
module JSON.Library (public, private) where

import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Library
import Test.HUnit


p ∷ (Value → Parser b) → ByteString → Maybe b
p f xs = case AP.parse json xs of
  AP.Done _ j → case parse f j of
    Success v → Just v
    _ → Nothing
  _ → Nothing


(..:) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(..:) = fmap . fmap


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())
instance Assertable (Either LastfmError (Maybe a)) where
  assert α = either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust) α


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "Library.addAlbum" $ TestCase testAddAlbum
  , TestLabel "Library.addArtist" $ TestCase testAddArtist
  , TestLabel "Library.addTrack" $ TestCase testAddTrack
  , TestLabel "Library.removeAlbum" $ TestCase testRemoveAlbum
  , TestLabel "Library.removeArtist" $ TestCase testRemoveArtist
  , TestLabel "Library.removeTrack" $ TestCase testRemoveTrack
  , TestLabel "Library.removeScrobble" $ TestCase testRemoveScrobble
  ]
 where
  testAddAlbum = assert $
    addAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") ak sk s

  testAddArtist = assert $
    addArtist (Artist "Mobthrow") ak sk s

  testAddTrack = assert $
    addTrack (Artist "Eminem") (Track "Kim") ak sk s

  testRemoveAlbum = assert $
    removeAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") ak sk s

  testRemoveArtist = assert $
    removeArtist (Artist "Burzum") ak sk s

  testRemoveTrack = assert $
    removeTrack (Artist "Eminem") (Track "Kim") ak sk s

  testRemoveScrobble = assert $
    removeScrobble (Artist "Gojira") (Track "Ocean") (Timestamp 1328905590) ak sk s


public ∷ [Test]
public =
  [ TestLabel "Library.getAlbums" $ TestCase testGetAlbums
  , TestLabel "Library.getArtists" $ TestCase testGetArtists
  , TestLabel "Library.getTracks" $ TestCase testGetTracks
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetAlbums = assert $
    p ga ..: getAlbums (User "smpcln") (Just $ Artist "Burzum") Nothing (Just $ Limit 5) ak

  testGetArtists = assert $
    p gar ..: getArtists (User "smpcln") Nothing (Just $ Limit 7) ak

  testGetTracks = assert $
    p gt ..: getTracks (User "smpcln") (Just $ Artist "Burzum") Nothing Nothing (Just $ Limit 4) ak


ga, gar, gt ∷ Value → Parser [String]
ga o = parseJSON o >>= (.: "albums") >>= (.: "album") >>= mapM (.: "name")
gar o = parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
