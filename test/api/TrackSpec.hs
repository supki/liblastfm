{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TrackSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Track
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Track.addTags" $
    query_ . sign privateSecret $
      addTags <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tags ["60s", "awesome"]
      <*> privateAPIKey <*> privateSessionKey

  it "Track.ban" $
    query_ . sign privateSecret $
      ban <*> artist "Eminem" <*> track "Kim"
      <*> privateAPIKey <*> privateSessionKey

  it "Track.love" $
    query_ . sign privateSecret $
      love <*> artist "Gojira" <*> track "Ocean"
      <*> privateAPIKey <*> privateSessionKey

  it "Track.removeTag" $
    query_ . sign privateSecret $
      removeTag <*> artist "Jefferson Airplane" <*> track "White rabbit" <*> tag "awesome"
      <*> privateAPIKey <*> privateSessionKey

  it "Track.share" $
    query_ . sign privateSecret $
      share <*> artist "Led Zeppelin" <*> track "When the Levee Breaks" <*> recipient "liblastfm" <* message "Just listen!"
      <*> privateAPIKey <*> privateSessionKey

  it "Track.unban" $
    query_ . sign privateSecret $
      unban <*> artist "Eminem" <*> track "Kim"
      <*> privateAPIKey <*> privateSessionKey

  it "Track.unlove" $
    query_ . sign privateSecret $
      unlove <*> artist "Gojira" <*> track "Ocean"
      <*> privateAPIKey <*> privateSessionKey

  it "Track.scrobble" $
    query ss . sign privateSecret $
      scrobble (pure (item <*> artist "Gojira" <*> track "Ocean" <*> timestamp 1300000000))
      <*> privateAPIKey <*> privateSessionKey

  it "Track.updateNowPlaying" $
    query np . sign privateSecret $
      updateNowPlaying <*> artist "Gojira" <*> track "Ocean"
      <*> privateAPIKey <*> privateSessionKey

  it "Track.getBuylinks" $
    query gbl $
      getBuyLinks <*> country "United Kingdom" <*> artist "Pink Floyd" <*> track "Brain Damage" <*> publicKey

  it "Track.getCorrection" $
    query gc $
      getCorrection <*> artist "Pink Ployd" <*> track "Brain Damage" <*> publicKey

  it "Track.getFingerprintMetadata" $
    query gfm $
      getFingerprintMetadata <*> fingerprint 1234 <*> publicKey

  it "Track.getInfo" $
    query gi $
      getInfo <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* username "aswalrus" <*> publicKey

  it "Track.getInfo_mbid" $
    query gi $
      getInfo <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* username "aswalrus" <*> publicKey

  it "Track.getShouts" $
    query gsh $
      getShouts <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 7 <*> publicKey

  it "Track.getShouts_mbid" $
    query gsh $
      getShouts <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 7 <*> publicKey

  it "Track.getSimilar" $
    query gsi $
      getSimilar <*> artist "Pink Floyd" <*> track "Comfortably Numb" <* limit 4 <*> publicKey

  it "Track.getSimilar_mbid" $
    query gsi $
      getSimilar <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <* limit 4 <*> publicKey

  it "Track.getTags" $
    query gt $
      getTags <*> artist "Jefferson Airplane" <*> track "White Rabbit" <* user "liblastfm" <*> publicKey

  it "Track.getTags_mbid" $
    query gt $
      getTags <*> mbid "001b3337-faf4-421a-a11f-45e0b60a7703"  <* user "liblastfm" <*> publicKey

  it "Track.getTopFans" $
    query gtf $
      getTopFans <*> artist "Pink Floyd" <*> track "Comfortably Numb" <*> publicKey

  it "Track.getTopFans_mbid" $
    query gtf $
      getTopFans <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <*> publicKey

  it "Track.getTopTags" $
    query gtt $
      getTopTags <*> artist "Pink Floyd" <*> track "Comfortably Numb" <*> publicKey

  it "Track.getTopTags_mbid" $
    query gtt $
      getTopTags <*> mbid "52d7c9ff-6ae4-48a6-acec-4c1a486f8c92" <*> publicKey

  it "Track.search" $
    query s' $
      search <*> track "Believe" <* limit 12 <*> publicKey

gbl, gc, gfm, gi, gsh, gsi, gt, gtf, gtt, np, s', ss :: Query Text
gbl = key "affiliations".key "downloads".key "affiliation".values.key "supplierName"._String
gc  = key "corrections".key "correction".key "track".key "artist".key "name"._String
gfm = key "tracks".key "track".values.key "name"._String
gi  = key "track".key "userplaycount"._String
gsh = key "shouts".key "shout".values.key "author"._String
gsi = key "similartracks".key "track".values.key "name"._String
gt  = key "tags".key "@attr".key "track"._String
gtf = key "topfans".key "user".values.key "name"._String
gtt = key "toptags".key "tag".values.key "name"._String
s'  = key "results".key "trackmatches".key "track".values.key "name"._String
ss  = key "scrobbles".key "scrobble".key "track".key "#text"._String
np  = key "nowplaying".key "track".key "#text"._String
