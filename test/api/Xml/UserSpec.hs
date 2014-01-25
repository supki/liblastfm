{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.UserSpec (spec) where

import Network.Lastfm
import Network.Lastfm.User
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "getRecentStations" $
    privately (getRecentStations <*> user "liblastfm" <* limit 10)
   `shouldHaveXml`
    root.node "recentstations".node "station".node "name".text

  it "getRecommendedArtists" $
    privately (getRecommendedArtists <* limit 10)
   `shouldHaveXml`
    root.node "recommendations".node "artist".node "name".text

  it "getRecommendedEvents" $
    privately (getRecommendedEvents <* limit 10)
   `shouldHaveXml`
    root.node "events".node "event".node "url".text

  it "shout" $
    shouldHaveXml_ . privately $
      shout <*> user "liblastfm" <*> message "test message"

  it "getArtistTracks" $
    publicly (getArtistTracks <*> user "smpcln" <*> artist "Dvar")
   `shouldHaveXml`
    root.node "artisttracks".node "track".node "name".text

  it "getBannedTracks" $
    publicly (getBannedTracks <*> user "smpcln" <* limit 10)
   `shouldHaveXml`
    root.node "bannedtracks".node "track".node "name".text

  it "getEvents" $
    publicly (getEvents <*> user "chansonnier" <* limit 5)
   `shouldHaveXml`
    root.node "events".node "event".node "venue".node "url".text

  it "getFriends" $
    publicly (getFriends <*> user "smpcln" <* limit 10)
   `shouldHaveXml`
    root.node "friends".node "user".node "name".text

  it "getPlayCount" $
    publicly (getInfo <*> user "smpcln")
   `shouldHaveXml`
    root.node "user".node "playcount".text

  it "getGetLovedTracks" $
    publicly (getLovedTracks <*> user "smpcln" <* limit 10)
   `shouldHaveXml`
    root.node "lovedtracks".node "track".node "name".text

  it "getNeighbours" $
    publicly (getNeighbours <*> user "smpcln" <* limit 10)
   `shouldHaveXml`
    root.node "neighbours".node "user".node "name".text

  it "getNewReleases" $
    publicly (getNewReleases <*> user "rj")
   `shouldHaveXml`
    root.node "albums".node "album".node "url".text

  it "getPastEvents" $
    publicly (getPastEvents <*> user "mokele" <* limit 5)
   `shouldHaveXml`
    root.node "events".node "event".node "url".text

  it "getPersonalTags" $
    publicly (getPersonalTags <*> user "crackedcore" <*> tag "rhythmic noise" <*> taggingType "artist" <* limit 10)
   `shouldHaveXml`
    root.node "taggings".node "artists".node "artist".node "name".text

  it "getPlaylists" $
    publicly (getPlaylists <*> user "mokele")
   `shouldHaveXml`
    root.node "playlists".node "playlist".node "title".text

  it "getRecentTracks" $
    publicly (getRecentTracks <*> user "smpcln" <* limit 10)
   `shouldHaveXml`
    root.node "recenttracks".node "track".node "name".text

  it "getShouts" $
    publicly (getShouts <*> user "smpcln" <* limit 2)
   `shouldHaveXml`
    root.node "shouts".node "shout".node "body".text

  it "getTopAlbums" $
    publicly (getTopAlbums <*> user "smpcln" <* limit 5)
   `shouldHaveXml`
    root.node "topalbums".node "album".node "artist".node "name".text

  it "getTopArtists" $
    publicly (getTopArtists <*> user "smpcln" <* limit 5)
   `shouldHaveXml`
    root.node "topartists".node "artist".node "name".text

  it "getTopTags" $
    publicly (getTopTags <*> user "smpcln" <* limit 10)
   `shouldHaveXml`
    root.node "toptags".node "tag".node "name".text

  it "getTopTracks" $
    publicly (getTopTracks <*> user "smpcln" <* limit 10)
   `shouldHaveXml`
    root.node "toptracks".node "track".node "url".text

  it "getWeeklyAlbumChart" $
    publicly (getWeeklyAlbumChart <*> user "smpcln")
   `shouldHaveXml`
    root.node "weeklyalbumchart".node "album".node "url".text

  it "getWeeklyArtistChart" $
    publicly (getWeeklyArtistChart <*> user "smpcln")
   `shouldHaveXml`
    root.node "weeklyartistchart".node "artist".node "url".text

  it "getWeeklyChartList" $
    publicly (getWeeklyChartList <*> user "smpcln")
   `shouldHaveXml`
    root.node "weeklychartlist".node "chart".attr "from"

  it "getWeeklyTrackChart" $
    publicly (getWeeklyTrackChart <*> user "smpcln")
   `shouldHaveXml`
    root.node "weeklytrackchart".node "track".node "url".text
