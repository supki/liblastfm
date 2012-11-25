{-# LANGUAGE FlexibleInstances #-}
module JSON.User (private, public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.User
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
  [ TestLabel "User.getRecentStations" $ TestCase testGetRecentStations
  , TestLabel "User.getRecommendedArtists" $ TestCase testGetRecommendedArtists
  , TestLabel "User.getRecommendedEvents" $ TestCase testGetRecommendedEvents
  , TestLabel "User.shout" $ TestCase testShout
  ]
 where
  testGetRecentStations = assert $
    p grs ..: getRecentStations (User "liblastfm") Nothing (Just $ Limit 10) ak sk s

  testGetRecommendedArtists = assert $
    p gra ..: getRecommendedArtists Nothing (Just $ Limit 10) ak sk s

  testGetRecommendedEvents = assert $
    p gre ..: getRecommendedEvents Nothing (Just $ Limit 10) ak sk s

  testShout = assert $
    shout (User "liblastfm") (Message "test message") ak sk s


public ∷ [Test]
public =
  [ TestLabel "User.getArtistTracks" $ TestCase testGetArtistTracks
  , TestLabel "User.getBannedTracks" $ TestCase testGetBannedTracks
  , TestLabel "User.getEvents" $ TestCase testGetEvents
  , TestLabel "User.getFriends" $ TestCase testGetFriends
  , TestLabel "User.getPlayCount" $ TestCase testGetPlayCount
  , TestLabel "User.getGetLovedTracks" $ TestCase testGetLovedTracks
  , TestLabel "User.getNeighbours" $ TestCase testGetNeighbours
  , TestLabel "User.getNewReleases" $ TestCase testGetNewReleases
  , TestLabel "User.getPastEvents" $ TestCase testGetPastEvents
  , TestLabel "User.getPersonalTags" $ TestCase testGetPersonalTags
  , TestLabel "User.getPlaylists" $ TestCase testGetPlaylists
  , TestLabel "User.getRecentTracks" $ TestCase testGetRecentTracks
  , TestLabel "User.getShouts" $ TestCase testGetShouts
  , TestLabel "User.getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "User.getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "User.getTopTags" $ TestCase testGetTopTags
  , TestLabel "User.getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "User.getWeeklyAlbumChart" $ TestCase testGetWeeklyAlbumChart
  , TestLabel "User.getWeeklyArtistChart" $ TestCase testGetWeeklyArtistChart
  , TestLabel "User.getWeeklyChartList" $ TestCase testGetWeeklyChartList
  , TestLabel "User.getWeeklyTrackChart" $ TestCase testGetWeeklyTrackChart
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetArtistTracks = assert $
    p gat ..: getArtistTracks (User "smpcln") (Artist "Dvar") Nothing Nothing Nothing ak

  testGetBannedTracks = assert $
    p gbt ..: getBannedTracks (User "smpcln") Nothing (Just $ Limit 10) ak

  testGetEvents = assert $
    p ge ..: getEvents (User "chansonnier") Nothing (Just $ Limit 5) Nothing ak

  testGetFriends = assert $
    p gf ..: getFriends (User "smpcln") Nothing Nothing (Just $ Limit 10) ak

  testGetPlayCount = assert $
    p gpc ..: getInfo (Just (User "smpcln")) ak

  testGetLovedTracks = assert $
    p glt ..: getLovedTracks (User "smpcln") Nothing (Just $ Limit 10) ak

  testGetNeighbours = assert $
    p gn ..: getNeighbours (User "smpcln") (Just $ Limit 10) ak

  testGetNewReleases = assert $
    p gnr ..: getNewReleases (User "smpcln") Nothing ak

  testGetPastEvents = assert $
    p gpe ..: getPastEvents (User "mokele") Nothing (Just $ Limit 5) ak

  testGetPersonalTags = assert $
    p gpt ..: getPersonalTags (User "crackedcore") (Tag "rhythmic noise") (TaggingType "artist") Nothing (Just $ Limit 10) ak

  testGetPlaylists = assert $
    p gp ..: getPlaylists (User "mokele") ak

  testGetRecentTracks = assert $
    p grt ..: getRecentTracks (User "smpcln") Nothing (Just $ Limit 10) Nothing Nothing ak

  testGetShouts = assert $
    p gs ..: getShouts (User "smpcln") Nothing (Just $ Limit 2) ak

  testGetTopAlbums = assert $
    p gtal ..: getTopAlbums (User "smpcln") Nothing Nothing (Just $ Limit 5) ak

  testGetTopArtists = assert $
    p gtar ..: getTopArtists (User "smpcln") Nothing Nothing (Just $ Limit 5) ak

  testGetTopTags = assert $
    p gtta ..: getTopTags (User "smpcln") (Just $ Limit 10) ak

  testGetTopTracks = assert $
    p gttr ..: getTopTracks (User "smpcln") Nothing Nothing (Just $ Limit 10) ak

  testGetWeeklyAlbumChart = assert $
    p gwalc ..: getWeeklyAlbumChart (User "rj") Nothing Nothing ak

  testGetWeeklyArtistChart = assert $
    p gwarc ..: getWeeklyArtistChart (User "rj") Nothing Nothing ak

  testGetWeeklyChartList = assert $
    p gwcl ..: getWeeklyChartList (User "rj") ak

  testGetWeeklyTrackChart = assert $
    p gwtc ..: getWeeklyTrackChart (User "rj") Nothing Nothing ak


gpc ∷ Value → Parser String
gat, gbt, ge, gf, glt, gn, gnr, gp, gpe, gpt, gra, gre, grs, grt, gs, gtal, gtar, gtta, gttr, gwalc, gwarc, gwcl, gwtc ∷ Value → Parser [String]
gat o = parseJSON o >>= (.: "artisttracks") >>= (.: "track") >>= mapM (.: "name")
gbt o = parseJSON o >>= (.: "bannedtracks") >>= (.: "track") >>= mapM (.: "name")
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "venue") >>= mapM (.: "url")
gf o = parseJSON o >>= (.: "friends") >>= (.: "user") >>= mapM (.: "name")
glt o = parseJSON o >>= (.: "lovedtracks") >>= (.: "track") >>= mapM (.: "name")
gn o = parseJSON o >>= (.: "neighbours") >>= (.: "user") >>= mapM (.: "name")
gnr o = parseJSON o >>= (.: "albums") >>= (.: "album") >>= mapM (.: "url")
gp o = parseJSON o >>=  (.: "playlists") >>= (.: "playlist") >>= mapM (.: "title")
gpc o = parseJSON o >>= (.: "user") >>= (.: "playcount")
gpe o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "url")
gpt o = parseJSON o >>= (.: "taggings") >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name")
gra o = parseJSON o >>= (.: "recommendations") >>= (.: "artist") >>= mapM (.: "name")
gre o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "url")
grs o = parseJSON o >>= (.: "recentstations") >>= (.: "station") >>= mapM (.: "name")
grt o = parseJSON o >>= (.: "recenttracks") >>= (.: "track") >>= mapM (.: "name")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body")
gtal o = parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "artist") >>= mapM (.: "name")
gtar o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")
gtta o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gttr o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "url")
gwalc o = parseJSON o >>= (.: "weeklyalbumchart") >>= (.: "album") >>= mapM (.: "url")
gwarc o = parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "url")
gwcl o = take 5 <$> (parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (.: "from"))
gwtc o = parseJSON o >>= (.: "weeklytrackchart") >>= (.: "track") >>= mapM (.: "url")
