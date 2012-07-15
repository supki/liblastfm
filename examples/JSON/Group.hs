{-# LANGUAGE FlexibleInstances #-}
module JSON.Group (public) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Group
import Test.HUnit


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool ("Cannot parse JSON") . isJust . β)


public ∷ [Test]
public =
  [ TestLabel "getHype" $ TestCase testGetHype
  , TestLabel "getMembers" $ TestCase testGetMembers
  , TestLabel "getWeeklyAlbumChart" $ TestCase testGetWeeklyAlbumChart
  , TestLabel "getWeeklyArtistChart" $ TestCase testGetWeeklyArtistChart
  , TestLabel "getWeeklyChartList" $ TestCase testGetWeeklyChartList
  , TestLabel "getWeeklyTrackChart" $ TestCase testGetWeeklyTrackChart
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"
  g = Group "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"

  testGetHype = assert
    (getHype g ak, decode ∷ Response → Maybe GH)

  testGetMembers = assert
    (getMembers g Nothing (Just $ Limit 10) ak, decode ∷ Response → Maybe GM)

  testGetWeeklyAlbumChart = assert
    (getWeeklyAlbumChart g Nothing Nothing ak, decode ∷ Response → Maybe GA)

  testGetWeeklyArtistChart = assert
    (getWeeklyArtistChart g Nothing Nothing ak, decode ∷ Response → Maybe GAR)

  testGetWeeklyChartList = assert
    (getWeeklyChartList g ak, decode ∷ Response → Maybe GC)

  testGetWeeklyTrackChart = assert
    (getWeeklyTrackChart g Nothing Nothing ak, decode ∷ Response → Maybe GT)


newtype GA = GA [String] deriving Show
newtype GAR = GAR [String] deriving Show
newtype GC = GC [(String,String)] deriving Show
newtype GH = GH [String] deriving Show
newtype GM = GM [String] deriving Show
newtype GT = GT [String] deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "weeklyalbumchart") >>= (.: "album") >>= mapM (.: "playcount"))
instance FromJSON GAR where
  parseJSON o = GAR <$> (parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GC where
  parseJSON o = GC <$> (parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to")))
instance FromJSON GH where
  parseJSON o = GH <$> (parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "mbid"))
instance FromJSON GM where
  parseJSON o = GM <$> (parseJSON o >>= (.: "members") >>= (.: "user") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "weeklytrackchart") >>= (.: "track") >>= mapM (.: "url"))
