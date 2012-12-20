{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module User (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.User
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ testCase "User.getRecentStations" testGetRecentStations
  , testCase "User.getRecommendedArtists" testGetRecommendedArtists
  , testCase "User.getRecommendedEvents" testGetRecommendedEvents
  , testCase "User.shout" testShout
  ]
 where
  testGetRecentStations = check grs . sign s $
    getRecentStations <*> user "liblastfm" <* limit 10 <*> apiKey ak <*> sessionKey sk

  testGetRecommendedArtists = check gra . sign s $
    getRecommendedArtists <* limit 10 <*> apiKey ak <*> sessionKey sk

  testGetRecommendedEvents = check gre . sign s $
    getRecommendedEvents <* limit 10 <*> apiKey ak <*> sessionKey sk

  testShout = check ok . sign s $
    shout <*> user "liblastfm" <*> message "test message" <*> apiKey ak <*> sessionKey sk


noauth ∷ [Test]
noauth =
  [ testCase "User.getArtistTracks" testGetArtistTracks
  , testCase "User.getBannedTracks" testGetBannedTracks
  , testCase "User.getEvents" testGetEvents
  , testCase "User.getFriends" testGetFriends
  , testCase "User.getPlayCount" testGetPlayCount
  , testCase "User.getGetLovedTracks" testGetLovedTracks
  , testCase "User.getNeighbours" testGetNeighbours
  , testCase "User.getNewReleases" testGetNewReleases
  , testCase "User.getPastEvents" testGetPastEvents
  , testCase "User.getPersonalTags" testGetPersonalTags
  , testCase "User.getPlaylists" testGetPlaylists
  , testCase "User.getRecentTracks" testGetRecentTracks
  , testCase "User.getShouts" testGetShouts
  , testCase "User.getTopAlbums" testGetTopAlbums
  , testCase "User.getTopArtists" testGetTopArtists
  , testCase "User.getTopTags" testGetTopTags
  , testCase "User.getTopTracks" testGetTopTracks
  , testCase "User.getWeeklyAlbumChart" testGetWeeklyAlbumChart
  , testCase "User.getWeeklyArtistChart" testGetWeeklyArtistChart
  , testCase "User.getWeeklyChartList" testGetWeeklyChartList
  , testCase "User.getWeeklyTrackChart" testGetWeeklyTrackChart
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetArtistTracks = check gat $
    getArtistTracks <*> user "smpcln" <*> artist "Dvar" <*> apiKey ak

  testGetBannedTracks = check gbt $
    getBannedTracks <*> user "smpcln" <* limit 10 <*> apiKey ak

  testGetEvents = check ge $
    getEvents <*> user "chansonnier" <* limit 5 <*> apiKey ak

  testGetFriends = check gf $
    getFriends <*> user "smpcln" <* limit 10 <*> apiKey ak

  testGetPlayCount = check gpc $
    getInfo <*> user "smpcln" <*> apiKey ak

  testGetLovedTracks = check glt $
    getLovedTracks <*> user "smpcln" <* limit 10 <*> apiKey ak

  testGetNeighbours = check gn $
    getNeighbours <*> user "smpcln" <* limit 10 <*> apiKey ak

  testGetNewReleases = check gnr $
    getNewReleases <*> user "smpcln" <*> apiKey ak

  testGetPastEvents = check gpe $
    getPastEvents <*> user "mokele" <* limit 5 <*> apiKey ak

  testGetPersonalTags = check gpt $
    getPersonalTags <*> user "crackedcore" <*> tag "rhythmic noise" <*> taggingType "artist" <* limit 10 <*> apiKey ak

  testGetPlaylists = check gp $
    getPlaylists <*> user "mokele" <*> apiKey ak

  testGetRecentTracks = check grt $
    getRecentTracks <*> user "smpcln" <* limit 10 <*> apiKey ak

  testGetShouts = check gs $
    getShouts <*> user "smpcln" <* limit 2 <*> apiKey ak

  testGetTopAlbums = check gtal $
    getTopAlbums <*> user "smpcln" <* limit 5 <*> apiKey ak

  testGetTopArtists = check gtar $
    getTopArtists <*> user "smpcln" <* limit 5 <*> apiKey ak

  testGetTopTags = check gtta $
    getTopTags <*> user "smpcln" <* limit 10 <*> apiKey ak

  testGetTopTracks = check gttr $
    getTopTracks <*> user "smpcln" <* limit 10 <*> apiKey ak

  testGetWeeklyAlbumChart = check gwalc $
    getWeeklyAlbumChart <*> user "rj" <*> apiKey ak

  testGetWeeklyArtistChart = check gwarc $
    getWeeklyArtistChart <*> user "rj" <*> apiKey ak

  testGetWeeklyChartList = check gwcl $
    getWeeklyChartList <*> user "rj" <*> apiKey ak

  testGetWeeklyTrackChart = check gwtc $
    getWeeklyTrackChart <*> user "rj" <*> apiKey ak


gpc ∷ Value → Parser String
gat, gbt, ge, gf, glt, gn, gnr, gp, gpe, gpt, gra, gre, grs, grt, gs, gtal, gtar, gtta, gttr, gwalc, gwarc, gwcl, gwtc ∷ Value → Parser [String]
gat o = parseJSON o >>= (.: "artisttracks") >>= (.: "track") >>= mapM (.: "name")
gbt o = parseJSON o >>= (.: "bannedtracks") >>= (.: "track") >>= mapM (.: "name")
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "venue") >>= mapM (.: "url")
gf o = parseJSON o >>= (.: "friends") >>= (.: "user") >>= mapM (.: "name")
glt o = parseJSON o >>= (.: "lovedtracks") >>= (.: "track") >>= mapM (.: "name")
gn o = parseJSON o >>= (.: "neighbours") >>= (.: "user") >>= mapM (.: "name")
gnr o = parseJSON o >>= (.: "albums") >>= (.: "album") >>= mapM (.: "url")
gp o = parseJSON o >>= (.: "playlists") >>= (.: "playlist") >>= mapM (.: "title")
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
gwcl o = take 5 `fmap` (parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (.: "from"))
gwtc o = parseJSON o >>= (.: "weeklytrackchart") >>= (.: "track") >>= mapM (.: "url")
