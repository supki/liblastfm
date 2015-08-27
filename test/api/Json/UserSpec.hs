{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.UserSpec (spec) where

import Data.Aeson.Lens
import Lastfm
import Lastfm.User
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "getRecentStations" $
    privately (getRecentStations <*> user "liblastfm" <* limit 10)
   `shouldHaveJson`
    key "recentstations".key "station".values.key "name"._String

  it "getRecommendedArtists" $
    privately (getRecommendedArtists <* limit 10)
   `shouldHaveJson`
    key "recommendations".key "artist".values.key "name"._String

  it "getRecommendedEvents" $
    privately (getRecommendedEvents <* limit 10)
   `shouldHaveJson`
    key "events".key "event".key "url"._String

  it "shout" $
    shouldHaveJson_ . privately $
      shout <*> user "liblastfm" <*> message "test message"

  it "getArtistTracks" $
    publicly (getArtistTracks <*> user "smpcln" <*> artist "Dvar")
   `shouldHaveJson`
    key "artisttracks".key "track".values.key "name"._String

  it "getBannedTracks" $
    publicly (getBannedTracks <*> user "smpcln" <* limit 10)
   `shouldHaveJson`
    key "bannedtracks".key "track".values.key "name"._String

  it "getEvents" $
    publicly (getEvents <*> user "chansonnier" <* limit 5)
   `shouldHaveJson`
    key "events".key "event".values.key "venue".key "url"._String

  it "getFriends" $
    publicly (getFriends <*> user "smpcln" <* limit 10)
   `shouldHaveJson`
    key "friends".key "user".values.key "name"._String

  it "getPlayCount" $
    publicly (getInfo <*> user "smpcln")
   `shouldHaveJson`
    key "user".key "playcount"._String

  it "getGetLovedTracks" $
    publicly (getLovedTracks <*> user "smpcln" <* limit 10)
   `shouldHaveJson`
    key "lovedtracks".key "track".values.key "name"._String

  it "getNeighbours" $
    publicly (getNeighbours <*> user "smpcln" <* limit 10)
   `shouldHaveJson`
    key "neighbours".key "user".values.key "name"._String

  it "getNewReleases" $
    publicly (getNewReleases <*> user "rj")
   `shouldHaveJson`
    key "albums".key "album".values.key "url"._String

  it "getPastEvents" $
    publicly (getPastEvents <*> user "mokele" <* limit 5)
   `shouldHaveJson`
    key "events".key "event".values.key "url"._String

  it "getPersonalTags" $
    publicly (getPersonalTags <*> user "crackedcore" <*> tag "rhythmic noise" <*> taggingType "artist" <* limit 10)
   `shouldHaveJson`
    key "taggings".key "artists".key "artist".values.key "name"._String

  it "getPlaylists" $
    publicly (getPlaylists <*> user "mokele")
   `shouldHaveJson`
    key "playlists".key "playlist".values.key "title"._String

  it "getRecentTracks" $
    publicly (getRecentTracks <*> user "smpcln" <* limit 10)
   `shouldHaveJson`
    key "recenttracks".key "track".values.key "name"._String

  it "getShouts" $
    publicly (getShouts <*> user "smpcln" <* limit 2)
   `shouldHaveJson`
    key "shouts".key "shout".values.key "body"._String

  it "getTopAlbums" $
    publicly (getTopAlbums <*> user "smpcln" <* limit 5)
   `shouldHaveJson`
    key "topalbums".key "album".values.key "artist".key "name"._String

  it "getTopArtists" $
    publicly (getTopArtists <*> user "smpcln" <* limit 5)
   `shouldHaveJson`
    key "topartists".key "artist".values.key "name"._String

  it "getTopTags" $
    publicly (getTopTags <*> user "smpcln" <* limit 10)
   `shouldHaveJson`
    key "toptags".key "tag".values.key "name"._String

  it "getTopTracks" $
    publicly (getTopTracks <*> user "smpcln" <* limit 10)
   `shouldHaveJson`
    key "toptracks".key "track".values.key "url"._String

  it "getWeeklyAlbumChart" $
    publicly (getWeeklyAlbumChart <*> user "smpcln")
   `shouldHaveJson`
    key "weeklyalbumchart".key "album".values.key "url"._String

  it "getWeeklyArtistChart" $
    publicly (getWeeklyArtistChart <*> user "smpcln")
   `shouldHaveJson`
    key "weeklyartistchart".key "artist".values.key "url"._String

  it "getWeeklyChartList" $
    publicly (getWeeklyChartList <*> user "smpcln")
   `shouldHaveJson`
    key "weeklychartlist".key "chart".values.key "from"._String

  it "getWeeklyTrackChart" $
    publicly (getWeeklyTrackChart <*> user "smpcln")
   `shouldHaveJson`
    key "weeklytrackchart".key "track".values.key "url"._String
