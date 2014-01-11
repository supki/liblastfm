{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module User (auth, noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.User
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "User.getRecentStations" testGetRecentStations
  , testCase "User.getRecommendedArtists" testGetRecommendedArtists
  , testCase "User.getRecommendedEvents" testGetRecommendedEvents
  , testCase "User.shout" testShout
  ]
 where
  testGetRecentStations = query grs . sign s $
    getRecentStations <*> user "liblastfm" <* limit 10 <*> ak <*> sk

  testGetRecommendedArtists = query gra . sign s $
    getRecommendedArtists <* limit 10 <*> ak <*> sk

  testGetRecommendedEvents = query gre . sign s $
    getRecommendedEvents <* limit 10 <*> ak <*> sk

  testShout = query ok . sign s $
    shout <*> user "liblastfm" <*> message "test message" <*> ak <*> sk


noauth :: [Test]
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
  testGetArtistTracks = query gat $
    getArtistTracks <*> user "smpcln" <*> artist "Dvar" <*> publicKey

  testGetBannedTracks = query gbt $
    getBannedTracks <*> user "smpcln" <* limit 10 <*> publicKey

  testGetEvents = query ge $
    getEvents <*> user "chansonnier" <* limit 5 <*> publicKey

  testGetFriends = query gf $
    getFriends <*> user "smpcln" <* limit 10 <*> publicKey

  testGetPlayCount = query gpc $
    getInfo <*> user "smpcln" <*> publicKey

  testGetLovedTracks = query glt $
    getLovedTracks <*> user "smpcln" <* limit 10 <*> publicKey

  testGetNeighbours = query gn $
    getNeighbours <*> user "smpcln" <* limit 10 <*> publicKey

  testGetNewReleases = query gnr $
    getNewReleases <*> user "rj" <*> publicKey

  testGetPastEvents = query gpe $
    getPastEvents <*> user "mokele" <* limit 5 <*> publicKey

  testGetPersonalTags = query gpt $
    getPersonalTags <*> user "crackedcore" <*> tag "rhythmic noise" <*> taggingType "artist" <* limit 10 <*> publicKey

  testGetPlaylists = query gp $
    getPlaylists <*> user "mokele" <*> publicKey

  testGetRecentTracks = query grt $
    getRecentTracks <*> user "smpcln" <* limit 10 <*> publicKey

  testGetShouts = query gs $
    getShouts <*> user "smpcln" <* limit 2 <*> publicKey

  testGetTopAlbums = query gtal $
    getTopAlbums <*> user "smpcln" <* limit 5 <*> publicKey

  testGetTopArtists = query gtar $
    getTopArtists <*> user "smpcln" <* limit 5 <*> publicKey

  testGetTopTags = query gtta $
    getTopTags <*> user "smpcln" <* limit 10 <*> publicKey

  testGetTopTracks = query gttr $
    getTopTracks <*> user "smpcln" <* limit 10 <*> publicKey

  testGetWeeklyAlbumChart = query gwalc $
    getWeeklyAlbumChart <*> user "smpcln" <*> publicKey

  testGetWeeklyArtistChart = query gwarc $
    getWeeklyArtistChart <*> user "smpcln" <*> publicKey

  testGetWeeklyChartList = query gwcl $
    getWeeklyChartList <*> user "smpcln" <*> publicKey

  testGetWeeklyTrackChart = query gwtc $
    getWeeklyTrackChart <*> user "smpcln" <*> publicKey


gat, gbt, ge, gf, glt, gn, gnr, gp, gpe, gpt, gra, gpc, gre, grs, grt, gs, gtal, gtar, gtta, gttr, gwalc, gwarc, gwcl, gwtc :: Query Text
gat   = key "artisttracks".key "track".values.key "name"._String
gbt   = key "bannedtracks".key "track".values.key "name"._String
ge    = key "events".key "event".values.key "venue".key "url"._String
gf    = key "friends".key "user".values.key "name"._String
glt   = key "lovedtracks".key "track".values.key "name"._String
gn    = key "neighbours".key "user".values.key "name"._String
gnr   = key "albums".key "album".values.key "url"._String
gp    = key "playlists".key "playlist".values.key "title"._String
gpc   = key "user".key "playcount"._String
gpe   = key "events".key "event".values.key "url"._String
gpt   = key "taggings".key "artists".key "artist".values.key "name"._String
gra   = key "recommendations".key "artist".values.key "name"._String
gre   = key "events".key "event".key "url"._String
grs   = key "recentstations".key "station".values.key "name"._String
grt   = key "recenttracks".key "track".values.key "name"._String
gs    = key "shouts".key "shout".values.key "body"._String
gtal  = key "topalbums".key "album".values.key "artist".key "name"._String
gtar  = key "topartists".key "artist".values.key "name"._String
gtta  = key "toptags".key "tag".values.key "name"._String
gttr  = key "toptracks".key "track".values.key "url"._String
gwalc = key "weeklyalbumchart".key "album".values.key "url"._String
gwarc = key "weeklyartistchart".key "artist".values.key "url"._String
gwcl  = key "weeklychartlist".key "chart".values.key "from"._String
gwtc  = key "weeklytrackchart".key "track".values.key "url"._String
