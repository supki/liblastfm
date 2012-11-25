{-# LANGUAGE FlexibleInstances #-}
module JSON.Tag (public) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Tag
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
  [ TestLabel "Tag.getInfo" $ TestCase testGetInfo
  , TestLabel "Tag.getSimilar" $ TestCase testGetSimilar
  , TestLabel "Tag.getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "Tag.getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "Tag.getTopTags" $ TestCase testGetTopTags
  , TestLabel "Tag.getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "Tag.getWeeklyArtistChart" $ TestCase testGetWeeklyArtistChart
  , TestLabel "Tag.getWeeklyChartList" $ TestCase testGetWeeklyChartList
  , TestLabel "Tag.search" $ TestCase testSearch
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetInfo = assert $
    p gi ..: getInfo (Tag "depressive") Nothing ak

  testGetSimilar = assert $
    p gs ..: getSimilar (Tag "depressive") ak

  testGetTopAlbums = assert $
    p gta ..: getTopAlbums (Tag "depressive") Nothing (Just $ Limit 2) ak

  testGetTopArtists = assert $
    p gtar ..: getTopArtists (Tag "depressive") Nothing (Just $ Limit 3) ak

  testGetTopTags = assert $
    p gtt ..: getTopTags ak

  testGetTopTracks = assert $
    p gttr ..: getTopTracks (Tag "depressive") Nothing (Just $ Limit 2) ak

  testGetWeeklyArtistChart = assert $
    p gwac ..: getWeeklyArtistChart (Tag "depressive") Nothing Nothing (Just $ Limit 3) ak

  testGetWeeklyChartList = assert $
    p gc ..: getWeeklyChartList (Tag "depressive") ak

  testSearch = assert $
    p se ..: search (Tag "depressive") Nothing (Just $ Limit 3) ak


gi ∷ Value → Parser String
gs, gta, gtar, gtt, gttr, gwac, se ∷ Value → Parser [String]
gc ∷ Value → Parser [(String, String)]
gi o = parseJSON o >>= (.: "tag") >>= (.: "taggings")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to"))
gs o = parseJSON o >>= (.: "similartags") >>= (.: "tag") >>= mapM (.: "name")
gta o = parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "url")
gtar o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "url")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gttr o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "url")
gwac o = parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "name")
se o = parseJSON o >>= (.: "results") >>= (.: "tagmatches") >>= (.: "tag") >>= mapM (.: "name")
