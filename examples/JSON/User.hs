{-# LANGUAGE FlexibleInstances #-}
module JSON.User (private, public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.User
import Test.HUnit


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())
instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "getRecentStations" $ TestCase testGetRecentStations
  , TestLabel "getRecommendedArtists" $ TestCase testGetRecommendedArtists
  , TestLabel "getRecommendedEvents" $ TestCase testGetRecommendedEvents
  , TestLabel "shout" $ TestCase testShout
  ]
 where
  testGetRecentStations = assert
    (getRecentStations (User "liblastfm") Nothing (Just $ Limit 10) ak sk s, decode ∷ Response → Maybe GRS)

  testGetRecommendedArtists = assert
    (getRecommendedArtists Nothing (Just $ Limit 10) ak sk s, decode ∷ Response → Maybe GRA)

  testGetRecommendedEvents = assert
    (getRecommendedEvents Nothing (Just $ Limit 10) ak sk s, decode ∷ Response → Maybe GRE)

  testShout = assert $
    shout (User "liblastfm") (Message "test message") ak sk s


public ∷ [Test]
public =
  [ TestLabel "getArtistTracks" $ TestCase testGetArtistTracks
  , TestLabel "getBannedTracks" $ TestCase testGetBannedTracks
  , TestLabel "getEvents" $ TestCase testGetEvents
  , TestLabel "getFriends" $ TestCase testGetFriends
  , TestLabel "getPlayCount" $ TestCase testGetPlayCount
  , TestLabel "getGetLovedTracks" $ TestCase testGetLovedTracks
  , TestLabel "getNeighbours" $ TestCase testGetNeighbours
  , TestLabel "getNewReleases" $ TestCase testGetNewReleases
  , TestLabel "getPastEvents" $ TestCase testGetPastEvents
  , TestLabel "getPersonalTags" $ TestCase testGetPersonalTags
  , TestLabel "getPlaylists" $ TestCase testGetPlaylists
  , TestLabel "getRecentTracks" $ TestCase testGetRecentTracks
  , TestLabel "getShouts" $ TestCase testGetShouts
  , TestLabel "getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "getTopArtists" $ TestCase testGetTopArtists
  , TestLabel "getTopTags" $ TestCase testGetTopTags
  , TestLabel "getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "getWeeklyAlbumChart" $ TestCase testGetWeeklyAlbumChart
  , TestLabel "getWeeklyArtistChart" $ TestCase testGetWeeklyArtistChart
  , TestLabel "getWeeklyChartList" $ TestCase testGetWeeklyChartList
  , TestLabel "getWeeklyTrackChart" $ TestCase testGetWeeklyTrackChart
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetArtistTracks = assert
    (getArtistTracks (User "smpcln") (Artist "Dvar") Nothing Nothing Nothing ak, decode ∷ Response → Maybe GAT)

  testGetBannedTracks = assert
    (getBannedTracks (User "smpcln") Nothing (Just $ Limit 10) ak, decode ∷ Response → Maybe GBT)

  testGetEvents = assert
    (getEvents (User "chansonnier") Nothing (Just $ Limit 5) Nothing ak, decode ∷ Response → Maybe GE)

  testGetFriends = assert
    (getFriends (User "smpcln") Nothing Nothing (Just $ Limit 10) ak, decode ∷ Response → Maybe GF)

  testGetPlayCount = assert
    (getInfo (Just (User "smpcln")) ak, decode ∷ Response → Maybe GPC)

  testGetLovedTracks = assert
    (getLovedTracks (User "smpcln") Nothing (Just $ Limit 10) ak, decode ∷ Response → Maybe GLT)

  testGetNeighbours = assert
    (getNeighbours (User "smpcln") (Just $ Limit 10) ak, decode ∷ Response → Maybe GN)

  testGetNewReleases = assert
    (getNewReleases (User "smpcln") Nothing ak, decode ∷ Response → Maybe GNR)

  testGetPastEvents = assert
    (getPastEvents (User "mokele") Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe GPE)

  testGetPersonalTags = assert
    (getPersonalTags (User "crackedcore") (Tag "rhythmic noise") (TaggingType "artist") Nothing (Just $ Limit 10) ak, decode ∷ Response → Maybe GPT)

  testGetPlaylists = assert
    (getPlaylists (User "mokele") ak, decode ∷ Response → Maybe GP)

  testGetRecentTracks = assert
    (getRecentTracks (User "smpcln") Nothing (Just $ Limit 10) Nothing Nothing ak, decode ∷ Response → Maybe GRT)

  testGetShouts = assert
    (getShouts (User "smpcln") Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GS)

  testGetTopAlbums = assert
    (getTopAlbums (User "smpcln") Nothing Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe GTAL)

  testGetTopArtists = assert
    (getTopArtists (User "smpcln") Nothing Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe GTAR)

  testGetTopTags = assert
    (getTopTags (User "smpcln") (Just $ Limit 10) ak, decode ∷ Response → Maybe GTTA)

  testGetTopTracks = assert
    (getTopTracks (User "smpcln") Nothing Nothing (Just $ Limit 10) ak, decode ∷ Response → Maybe GTTR)

  testGetWeeklyAlbumChart = assert
    (getWeeklyAlbumChart (User "rj") Nothing Nothing ak, decode ∷ Response → Maybe GWALC)

  testGetWeeklyArtistChart = assert
    (getWeeklyArtistChart (User "rj") Nothing Nothing ak, decode ∷ Response → Maybe GWARC)

  testGetWeeklyChartList = assert
    (getWeeklyChartList (User "rj") ak, decode ∷ Response → Maybe GWCL)

  testGetWeeklyTrackChart = assert
    (getWeeklyTrackChart (User "rj") Nothing Nothing ak, decode ∷ Response → Maybe GWTC)


newtype GAT = GAT [String] deriving Show
newtype GBT = GBT [String] deriving Show
newtype GE = GE [String] deriving Show
newtype GF = GF [String] deriving Show
newtype GLT = GLT [String] deriving Show
newtype GN = GN [String] deriving Show
newtype GNR = GNR [String] deriving Show
newtype GP = GP [String] deriving Show
newtype GPC = GPC String deriving Show
newtype GPE = GPE [String] deriving Show
newtype GPT = GPT [String] deriving Show
newtype GRA = GRA [String] deriving Show
newtype GRE = GRE [String] deriving Show
newtype GRS = GRS [String] deriving Show
newtype GRT = GRT [String] deriving Show
newtype GS = GS [String] deriving Show
newtype GTAL = GTAL [String] deriving Show
newtype GTAR = GTAR [String] deriving Show
newtype GTTA = GTTA [String] deriving Show
newtype GTTR = GTTR [String] deriving Show
newtype GWALC = GWALC [String] deriving Show
newtype GWARC = GWARC [String] deriving Show
newtype GWCL = GWCL [String] deriving Show
newtype GWTC = GWTC [String] deriving Show


instance FromJSON GAT where
  parseJSON o = GAT <$> (parseJSON o >>= (.: "artisttracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GBT where
  parseJSON o = GBT <$> (parseJSON o >>= (.: "bannedtracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GE where
  parseJSON o = GE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "venue") >>= mapM (.: "url"))
instance FromJSON GF where
  parseJSON o = GF <$> (parseJSON o >>= (.: "friends") >>= (.: "user") >>= mapM (.: "name"))
instance FromJSON GLT where
  parseJSON o = GLT <$> (parseJSON o >>= (.: "lovedtracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GN where
  parseJSON o = GN <$> (parseJSON o >>= (.: "neighbours") >>= (.: "user") >>= mapM (.: "name"))
instance FromJSON GNR where
  parseJSON o = GNR <$> (parseJSON o >>= (.: "albums") >>= (.: "album") >>= mapM (.: "url"))
instance FromJSON GP where
  parseJSON o = GP <$> (parseJSON o >>=  (.: "playlists") >>= (.: "playlist") >>= mapM (.: "title"))
instance FromJSON GPC where
  parseJSON o = GPC <$> (parseJSON o >>= (.: "user") >>= (.: "playcount"))
instance FromJSON GPE where
  parseJSON o = GPE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "url"))
instance FromJSON GPT where
  parseJSON o = GPT <$> (parseJSON o >>= (.: "taggings") >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GRA where
  parseJSON o = GRA <$> (parseJSON o >>= (.: "recommendations") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GRE where
  parseJSON o = GRE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "url"))
instance FromJSON GRS where
  parseJSON o = GRS <$> (parseJSON o >>= (.: "recentstations") >>= (.: "station") >>= mapM (.: "name"))
instance FromJSON GRT where
  parseJSON o = GRT <$> (parseJSON o >>= (.: "recenttracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GS where
  parseJSON o = GS <$> (parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body"))
instance FromJSON GTAL where
  parseJSON o = GTAL <$> (parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "artist") >>= mapM (.: "name"))
instance FromJSON GTAR where
  parseJSON o = GTAR <$> (parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GTTA where
  parseJSON o = GTTA <$> (parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GTTR where
  parseJSON o = GTTR <$> (parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "url"))
instance FromJSON GWALC where
  parseJSON o = GWALC <$> (parseJSON o >>= (.: "weeklyalbumchart") >>= (.: "album") >>= mapM (.: "url"))
instance FromJSON GWARC where
  parseJSON o = GWARC <$> (parseJSON o >>= (.: "weeklyartistchart") >>= (.: "artist") >>= mapM (.: "url"))
instance FromJSON GWCL where
  parseJSON o = GWCL . take 5 <$> (parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (.: "from"))
instance FromJSON GWTC where
  parseJSON o = GWTC <$> (parseJSON o >>= (.: "weeklytrackchart") >>= (.: "track") >>= mapM (.: "url"))
