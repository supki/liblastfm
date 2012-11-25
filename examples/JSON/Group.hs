{-# LANGUAGE FlexibleInstances #-}
module JSON.Group (public) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Group
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
  [ TestLabel "Group.getHype" $ TestCase testGetHype
  , TestLabel "Group.getMembers" $ TestCase testGetMembers
  , TestLabel "Group.getWeeklyAlbumChart" $ TestCase testGetWeeklyAlbumChart
  , TestLabel "Group.getWeeklyArtistChart" $ TestCase testGetWeeklyArtistChart
  , TestLabel "Group.getWeeklyChartList" $ TestCase testGetWeeklyChartList
  , TestLabel "Group.getWeeklyTrackChart" $ TestCase testGetWeeklyTrackChart
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"
  g = Group "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"

  testGetHype = assert $
    p gh ..: getHype g ak

  testGetMembers = assert $
    p gm ..: getMembers g Nothing (Just $ Limit 10) ak

  testGetWeeklyAlbumChart = assert $
    p ga ..: getWeeklyAlbumChart g Nothing Nothing ak

  testGetWeeklyArtistChart = assert $
    p gar ..: getWeeklyArtistChart g Nothing Nothing ak

  testGetWeeklyChartList = assert $
    p gc ..: getWeeklyChartList g ak

  testGetWeeklyTrackChart = assert $
    p gt ..: getWeeklyTrackChart g Nothing Nothing ak


ga, gar, gh, gm, gt ∷ Value → Parser [String]
gc ∷ Value → Parser [(String, String)]
ga o = parseJSON o >>= (.: "weeklyalbumchart") >>= (.: "album") >>= mapM (.: "playcount")
gar o = parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "name")
gc o = parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to"))
gh o = parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "mbid")
gm o = parseJSON o >>= (.: "members") >>= (.: "user") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "weeklytrackchart") >>= (.: "track") >>= mapM (.: "url")
