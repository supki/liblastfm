{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module UserSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.User
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "User.getRecentStations" $
    query grs . sign privateSecret $
      getRecentStations <*> user "liblastfm" <* limit 10
      <*> privateAPIKey <*> privateSessionKey

  it "User.getRecommendedArtists" $
    query gra . sign privateSecret $
      getRecommendedArtists <* limit 10
      <*> privateAPIKey <*> privateSessionKey

  it "User.getRecommendedEvents" $
    query gre . sign privateSecret $
      getRecommendedEvents <* limit 10
      <*> privateAPIKey <*> privateSessionKey

  it "User.shout" $
    query_ . sign privateSecret $
      shout <*> user "liblastfm" <*> message "test message"
      <*> privateAPIKey <*> privateSessionKey

  it "User.getArtistTracks" $
    query gat $
      getArtistTracks <*> user "smpcln" <*> artist "Dvar" <*> publicKey

  it "User.getBannedTracks" $
    query gbt $
      getBannedTracks <*> user "smpcln" <* limit 10 <*> publicKey

  it "User.getEvents" $
    query ge $
      getEvents <*> user "chansonnier" <* limit 5 <*> publicKey

  it "User.getFriends" $
    query gf $
      getFriends <*> user "smpcln" <* limit 10 <*> publicKey

  it "User.getPlayCount" $
    query gpc $
      getInfo <*> user "smpcln" <*> publicKey

  it "User.getGetLovedTracks" $
    query glt $
      getLovedTracks <*> user "smpcln" <* limit 10 <*> publicKey

  it "User.getNeighbours" $
    query gn $
      getNeighbours <*> user "smpcln" <* limit 10 <*> publicKey

  it "User.getNewReleases" $
    query gnr $
      getNewReleases <*> user "rj" <*> publicKey

  it "User.getPastEvents" $
    query gpe $
      getPastEvents <*> user "mokele" <* limit 5 <*> publicKey

  it "User.getPersonalTags" $
    query gpt $
      getPersonalTags <*> user "crackedcore" <*> tag "rhythmic noise" <*> taggingType "artist" <* limit 10 <*> publicKey

  it "User.getPlaylists" $
    query gp $
      getPlaylists <*> user "mokele" <*> publicKey

  it "User.getRecentTracks" $
    query grt $
      getRecentTracks <*> user "smpcln" <* limit 10 <*> publicKey

  it "User.getShouts" $
    query gs $
      getShouts <*> user "smpcln" <* limit 2 <*> publicKey

  it "User.getTopAlbums" $
    query gtal $
      getTopAlbums <*> user "smpcln" <* limit 5 <*> publicKey

  it "User.getTopArtists" $
    query gtar $
      getTopArtists <*> user "smpcln" <* limit 5 <*> publicKey

  it "User.getTopTags" $
    query gtta $
      getTopTags <*> user "smpcln" <* limit 10 <*> publicKey

  it "User.getTopTracks" $
    query gttr $
      getTopTracks <*> user "smpcln" <* limit 10 <*> publicKey

  it "User.getWeeklyAlbumChart" $
    query gwalc $
      getWeeklyAlbumChart <*> user "smpcln" <*> publicKey

  it "User.getWeeklyArtistChart" $
    query gwarc $
      getWeeklyArtistChart <*> user "smpcln" <*> publicKey

  it "User.getWeeklyChartList" $
    query gwcl $
      getWeeklyChartList <*> user "smpcln" <*> publicKey

  it "User.getWeeklyTrackChart" $
    query gwtc $
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
