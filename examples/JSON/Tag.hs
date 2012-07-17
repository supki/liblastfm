{-# LANGUAGE FlexibleInstances #-}
module JSON.Tag (public) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Tag
import Test.HUnit


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


public ∷ [Test]
public =
  [ TestLabel "getInfo" $ TestCase testGetInfo
  , TestLabel "getSimilar" $ TestCase testGetSimilar
  , TestLabel "getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "getTopTags" $ TestCase testGetTopTags
  , TestLabel "getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "getWeeklyArtistChart" $ TestCase testGetWeeklyArtistChart
  , TestLabel "getWeeklyChartList" $ TestCase testGetWeeklyChartList
  , TestLabel "search" $ TestCase testSearch
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetInfo = assert
    (getInfo (Tag "depressive") Nothing ak, decode ∷ Response → Maybe GI)

  testGetSimilar = assert
    (getSimilar (Tag "depressive") ak, decode ∷ Response → Maybe GS)

  testGetTopAlbums = assert
    (getTopAlbums (Tag "depressive") Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GTA)

  testGetTopArtists = assert
    (getTopArtists (Tag "depressive") Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GTAR)

  testGetTopTags = assert
    (getTopTags ak, decode ∷ Response → Maybe GTT)

  testGetTopTracks = assert
    (getTopTracks (Tag "depressive") Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GTTR)

  testGetWeeklyArtistChart = assert
    (getWeeklyArtistChart (Tag "depressive") Nothing Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GWAC)

  testGetWeeklyChartList = assert
    (getWeeklyChartList (Tag "depressive") ak, decode ∷ Response → Maybe GC)

  testSearch = assert
    (search (Tag "depressive") Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe SE)


newtype GI = GI String deriving Show
newtype GC = GC [(String,String)] deriving Show
newtype GS = GS [String] deriving Show
newtype GTA = GTA [String] deriving Show
newtype GTAR = GTAR [String] deriving Show
newtype GTT = GTT [String] deriving Show
newtype GTTR = GTTR [String] deriving Show
newtype GWAC = GWAC [String] deriving Show
newtype SE = SE [String] deriving Show


instance FromJSON GI where
  parseJSON o = GI <$> (parseJSON o >>= (.: "tag") >>= (.: "taggings"))
instance FromJSON GC where
  parseJSON o = GC <$> (parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to")))
instance FromJSON GS where
  parseJSON o = GS <$> (parseJSON o >>= (.: "similartags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GTA where
  parseJSON o = GTA <$> (parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "url"))
instance FromJSON GTAR where
  parseJSON o = GTAR <$> (parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "url"))
instance FromJSON GTT where
  parseJSON o = GTT <$> (parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GTTR where
  parseJSON o = GTTR <$> (parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "url"))
instance FromJSON GWAC where
  parseJSON o = GWAC <$> (parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON SE where
  parseJSON o = SE <$> (parseJSON o >>= (.: "results") >>= (.: "tagmatches") >>= (.: "tag") >>= mapM (.: "name"))
