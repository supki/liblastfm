{-# LANGUAGE FlexibleInstances #-}
module JSON.Geo (public) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Geo
import Test.HUnit


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


public ∷ [Test]
public =
  [ TestLabel "getEvents" $ TestCase testGetEvents
  , TestLabel "getMetroArtistChart" $ TestCase testGetMetroArtistChart
  , TestLabel "getMetroHypeArtistChart" $ TestCase testGetMetroHypeArtistChart
  , TestLabel "getMetroHypeTrackChart" $ TestCase testGetMetroHypeTrackChart
  , TestLabel "getMetroTrackChart" $ TestCase testGetMetroTrackChart
  , TestLabel "getMetroUniqueArtistChart" $ TestCase testGetMetroUniqueArtistChart
  , TestLabel "getMetroUniqueTrackChart" $ TestCase testGetMetroUniqueTrackChart
  , TestLabel "getMetroWeeklyChartlist" $ TestCase testGetMetroWeeklyChartlist
  , TestLabel "getMetros" $ TestCase testGetMetros
  , TestLabel "getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "getTopTracks" $ TestCase testGetTopTracks
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetEvents = assert
    (getEvents Nothing Nothing (Just $ Location "Moscow") Nothing Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe GE)

  testGetMetroArtistChart = assert
    (getMetroArtistChart (Country "Russia") (Metro "Saint Petersburg") Nothing Nothing ak, decode ∷ Response → Maybe GA)

  testGetMetroHypeArtistChart = assert
    (getMetroHypeArtistChart (Country "United States") (Metro "New York") Nothing Nothing ak, decode ∷ Response → Maybe GA)

  testGetMetroHypeTrackChart = assert
    (getMetroHypeTrackChart (Country "Russia") (Metro "Ufa") Nothing Nothing ak, decode ∷ Response → Maybe GT)

  testGetMetroTrackChart = assert
    (getMetroTrackChart (Country "United States") (Metro "Boston") Nothing Nothing ak, decode ∷ Response → Maybe GT)

  testGetMetroUniqueArtistChart = assert
    (getMetroUniqueArtistChart (Country "Belarus") (Metro "Minsk") Nothing Nothing ak, decode ∷ Response → Maybe GA)

  testGetMetroUniqueTrackChart = assert
    (getMetroUniqueTrackChart (Country "Russia") (Metro "Moscow") Nothing Nothing ak, decode ∷ Response → Maybe GT)

  testGetMetroWeeklyChartlist = assert
    (getMetroWeeklyChartlist (Metro "Moscow") ak, decode ∷ Response → Maybe GC)

  testGetMetros = assert
    (getMetros (Just $ Country "Russia") ak, decode ∷ Response → Maybe GM)

  testGetTopArtists = assert
    (getTopArtists (Country "Belarus") Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GA)

  testGetTopTracks = assert
    (getTopTracks (Country "Ukraine") Nothing Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GT)


newtype GA = GA [String] deriving Show
newtype GC = GC [(String,String)] deriving Show
newtype GE = GE [String] deriving Show
newtype GM = GM [String] deriving Show
newtype GT = GT [String] deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GC where
  parseJSON o = GC <$> (parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to")))
instance FromJSON GE where
  parseJSON o = GE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "id"))
instance FromJSON GM where
  parseJSON o = GM <$> (parseJSON o >>= (.: "metros") >>= (.: "metro") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name"))
