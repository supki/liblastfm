{-# LANGUAGE FlexibleInstances #-}
module JSON.Geo (public) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Geo
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
  [ TestLabel "Geo.getEvents" $ TestCase testGetEvents
  , TestLabel "Geo.getMetroArtistChart" $ TestCase testGetMetroArtistChart
  , TestLabel "Geo.getMetroHypeArtistChart" $ TestCase testGetMetroHypeArtistChart
  , TestLabel "Geo.getMetroHypeTrackChart" $ TestCase testGetMetroHypeTrackChart
  , TestLabel "Geo.getMetroTrackChart" $ TestCase testGetMetroTrackChart
  , TestLabel "Geo.getMetroUniqueArtistChart" $ TestCase testGetMetroUniqueArtistChart
  , TestLabel "Geo.getMetroUniqueTrackChart" $ TestCase testGetMetroUniqueTrackChart
  , TestLabel "Geo.getMetroWeeklyChartlist" $ TestCase testGetMetroWeeklyChartlist
  , TestLabel "Geo.getMetros" $ TestCase testGetMetros
  , TestLabel "Geo.getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "Geo.getTopTracks" $ TestCase testGetTopTracks
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetEvents = assert $
    p ge ..: getEvents Nothing Nothing (Just $ Location "Moscow") Nothing Nothing (Just $ Limit 5) ak

  testGetMetroArtistChart = assert $
    p ga ..: getMetroArtistChart (Country "Russia") (Metro "Saint Petersburg") Nothing Nothing ak

  testGetMetroHypeArtistChart = assert $
    p ga ..: getMetroHypeArtistChart (Country "United States") (Metro "New York") Nothing Nothing ak

  testGetMetroHypeTrackChart = assert $
    p gt ..: getMetroHypeTrackChart (Country "Russia") (Metro "Ufa") Nothing Nothing ak

  testGetMetroTrackChart = assert $
    p gt ..: getMetroTrackChart (Country "United States") (Metro "Boston") Nothing Nothing ak

  testGetMetroUniqueArtistChart = assert $
    p ga ..: getMetroUniqueArtistChart (Country "Belarus") (Metro "Minsk") Nothing Nothing ak

  testGetMetroUniqueTrackChart = assert $
    p gt ..: getMetroUniqueTrackChart (Country "Russia") (Metro "Moscow") Nothing Nothing ak

  testGetMetroWeeklyChartlist = assert $
    p gc ..: getMetroWeeklyChartlist (Metro "Moscow") ak

  testGetMetros = assert $
    p gm ..: getMetros (Just $ Country "Russia") ak

  testGetTopArtists = assert $
    p ga ..: getTopArtists (Country "Belarus") Nothing (Just $ Limit 3) ak

  testGetTopTracks = assert $
    p gt ..: getTopTracks (Country "Ukraine") Nothing Nothing (Just $ Limit 2) ak


ga, ge, gm, gt ∷ Value → Parser [String]
gc ∷ Value → Parser [(String, String)]
ga o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to"))
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "id")
gm o = parseJSON o >>= (.: "metros") >>= (.: "metro") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name")
