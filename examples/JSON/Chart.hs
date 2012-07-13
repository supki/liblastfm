{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module JSON.Chart (tests) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Chart
import Test.HUnit


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


tests ∷ [Test]
tests =
  [ TestLabel "getHypedArtists" $ TestCase testGetHypedArtists
  , TestLabel "getHypedTracks" $ TestCase testGetHypedTracks
  , TestLabel "getLovedTracks" $ TestCase testGetLovedTracks
  , TestLabel "getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "getTopTags" $ TestCase testGetTopTags
  , TestLabel "getTopTracks" $ TestCase testGetTopTracks
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetHypedArtists = assert
    (getHypedArtists Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GA)

  testGetHypedTracks = assert
    (getHypedTracks Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GT)

  testGetLovedTracks = assert
    (getLovedTracks Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GT)

  testGetTopArtists = assert
    (getTopArtists Nothing (Just $ Limit 4) ak, decode ∷ Response → Maybe GA)

  testGetTopTags = assert
    (getTopTags Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe GTA)

  testGetTopTracks = assert
    (getTopTracks Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GT)


newtype GA = GA [String] deriving Show
newtype GT = GT [String] deriving Show
newtype GTA = GTA [String] deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GTA where
  parseJSON o = GTA <$> (parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name"))
