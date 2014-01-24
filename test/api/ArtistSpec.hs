{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module ArtistSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Artist
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Artist.addTags" $
    query_ . sign privateSecret $
      addTags <*> artist "Егор Летов" <*> tags ["russian", "black metal"]
      <*> privateAPIKey <*> privateSessionKey

  it "Artist.getTags-authenticated" $
    query gt . sign privateSecret $
      getTags <*> artist "Егор Летов"
      <*> privateAPIKey <* privateSessionKey

  it "Artist.removeTag" $
    query_ . sign privateSecret $
      removeTag <*> artist "Егор Летов" <*> tag "russian"
      <*> privateAPIKey <*> privateSessionKey

  it "Artist.share" $
    query_ . sign privateSecret $
      share <*> artist "Sleep" <*> recipient "liblastfm" <* message "Just listen!"
      <*> privateAPIKey <*> privateSessionKey

  it "Artist.getCorrection" $
    query gc $
      getCorrection <*> artist "Meshugah" <*> publicKey

  it "Artist.getEvents" $
    query ge $
      getEvents <*> artist "Meshuggah" <* limit 2 <*> publicKey

  it "Artist.getEvents_mbid" $
    query ge $
      getEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 2 <*> publicKey

  it "Artist.getInfo" $
    query gin $
      getInfo <*> artist "Meshuggah" <*> publicKey

  it "Artist.getInfo_mbid" $
    query gin $
      getInfo <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  it "Artist.getPastEvents" $
    query gpe $
      getPastEvents <*> artist "Meshuggah" <* autocorrect True <*> publicKey

  it "Artist.getPastEvents_mbid" $
    query gpe $
      getPastEvents <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* autocorrect True <*> publicKey

  it "Artist.getPodcast" $
    query gp $
      getPodcast <*> artist "Meshuggah" <*> publicKey

  it "Artist.getPodcast_mbid" $
    query gp $
      getPodcast <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  it "Artist.getShouts" $
    query gs $
      getShouts <*> artist "Meshuggah" <* limit 5 <*> publicKey

  it "Artist.getShouts_mbid" $
    query gs $
      getShouts <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 5 <*> publicKey

  it "Artist.getSimilar" $
    query gsi $
      getSimilar <*> artist "Meshuggah" <* limit 3 <*> publicKey

  it "Artist.getSimilar_mbid" $
    query gsi $
      getSimilar <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> publicKey

  it "Artist.getTags" $
    query gt $
      getTags <*> artist "Егор Летов" <* user "liblastfm" <*> publicKey

  it "Artist.getTags_mbid" $
    query gt $
      getTags <*> mbid "cfb3d32e-d095-4d63-946d-9daf06932180" <* user "liblastfm" <*> publicKey

  it "Artist.getTopAlbums" $
    query gta $
      getTopAlbums <*> artist "Meshuggah" <* limit 3 <*> publicKey

  it "Artist.getTopAlbums_mbid" $
    query gta $
      getTopAlbums <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> publicKey

  it "Artist.getTopFans" $
    query gtf $
      getTopFans <*> artist "Meshuggah" <*> publicKey

  it "Artist.getTopFans_mbid" $
    query gtf $
      getTopFans <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  it "Artist.getTopTags" $
    query gtt $
      getTopTags <*> artist "Meshuggah" <*> publicKey

  it "Artist.getTopTags_mbid" $
    query gtt $
      getTopTags <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <*> publicKey

  it "Artist.getTopTracks" $
    query gttr $
      getTopTracks <*> artist "Meshuggah" <* limit 3 <*> publicKey

  it "Artist.getTopTracks_mbid" $
    query gttr $
      getTopTracks <*> mbid "cf8b3b8c-118e-4136-8d1d-c37091173413" <* limit 3 <*> publicKey

  it "Artist.search" $
    query se $
      search <*> artist "Mesh" <* limit 3 <*> publicKey

gc, gin, gp :: Query Text
ge, gpe, gs, gsi, gt, gta, gtf, gtt, gttr, se :: Query Text
gc   = key "corrections".key "correction".key "artist".key "name"._String
ge   = key "events".key "artist"._String
gin  = key "artist".key "stats".key "listeners"._String
gp   = key "rss".key "channel".key "description"._String
gpe  = key "events".key "event".values.key "title"._String
gs   = key "shouts".key "shout".values.key "author"._String
gsi  = key "similarartists".key "artist".values.key "name"._String
gt   = key "tags".key "tag".values.key "name"._String
gta  = key "topalbums".key "album".values.key "name"._String
gtf  = key "topfans".key "user".values.key "name"._String
gtt  = key "toptags".key "tag".values.key "name"._String
gttr = key "toptracks".key "track".values.key "name"._String
se   = key "results".key "artistmatches".key "artist".values.key "name"._String
