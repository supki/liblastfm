{-# LANGUAGE FlexibleInstances #-}
module JSON.Chart (public) where

import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Chart
import Test.HUnit


p ∷ (Value → Parser b) → ByteString → Maybe b
p f xs = case AP.parse json xs of
  AP.Done _ j → case parse f j of
    Success v → Just v
    _ → Nothing
  _ → Nothing


(..:) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(..:) = fmap . fmap


instance Assertable (Either LastfmError (Maybe a)) where
  assert α = either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust) α


public ∷ [Test]
public =
  [ TestLabel "getHypedArtists" $ TestCase testGetHypedArtists
  , TestLabel "getHypedTracks" $ TestCase testGetHypedTracks
  , TestLabel "getLovedTracks" $ TestCase testGetLovedTracks
  , TestLabel "getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "getTopTags" $ TestCase testGetTopTags
  , TestLabel "getTopTracks" $ TestCase testGetTopTracks
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetHypedArtists = assert $
    p ga ..: getHypedArtists Nothing (Just $ Limit 3) ak

  testGetHypedTracks = assert $
    p gt ..: getHypedTracks Nothing (Just $ Limit 2) ak

  testGetLovedTracks = assert $
    p gt ..: getLovedTracks Nothing (Just $ Limit 3) ak

  testGetTopArtists = assert $
    p ga ..: getTopArtists Nothing (Just $ Limit 4) ak

  testGetTopTags = assert $
    p gta ..: getTopTags Nothing (Just $ Limit 5) ak

  testGetTopTracks = assert $
    p gt ..: getTopTracks Nothing (Just $ Limit 2) ak


ga, gt, gta ∷ Value → Parser [String]
ga o = parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
gta o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
