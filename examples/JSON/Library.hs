{-# LANGUAGE FlexibleInstances #-}
module JSON.Library (public, private) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Library
import Test.HUnit


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())
instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "addAlbum" $ TestCase testAddAlbum
  , TestLabel "addArtist" $ TestCase testAddArtist
  , TestLabel "addTrack" $ TestCase testAddTrack
  , TestLabel "removeAlbum" $ TestCase testRemoveAlbum
  , TestLabel "removeArtist" $ TestCase testRemoveArtist
  , TestLabel "removeTrack" $ TestCase testRemoveTrack
  , TestLabel "removeScrobble" $ TestCase testRemoveScrobble
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
  [ TestLabel "getAlbums" $ TestCase testGetAlbums
  , TestLabel "getArtists" $ TestCase testGetArtists
  , TestLabel "getTracks" $ TestCase testGetTracks
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetAlbums = assert $
    (getAlbums (User "smpcln") (Just $ Artist "Burzum") Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe GA)

  testGetArtists = assert $
    (getArtists (User "smpcln") Nothing (Just $ Limit 7) ak, decode ∷ Response → Maybe GAR)

  testGetTracks = assert $
    (getTracks (User "smpcln") (Just $ Artist "Burzum") Nothing Nothing (Just $ Limit 4) ak, decode ∷ Response → Maybe GT)


newtype GA = GA [String] deriving Show
newtype GAR = GAR [String] deriving Show
newtype GT = GT [String] deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "albums") >>= (.: "album") >>= mapM (.: "name"))
instance FromJSON GAR where
  parseJSON o = GAR <$> (parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name"))
