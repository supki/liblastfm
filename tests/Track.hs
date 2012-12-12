{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Track (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Track
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ testCase "Track.addTags" testAddTags
  , testCase "Track.ban" testBan
  , testCase "Track.love" testLove
  , testCase "Track.removeTag" testRemoveTag
  , testCase "Track.share" testShare
  , testCase "Track.unban" testUnban
  , testCase "Track.unlove" testUnlove
  , testCase "Track.scrobble" testScrobble
  , testCase "Track.updateNowPlaying" testUpdateNowPlaying
  ]
 where
  testAddTags = check ok . sign s $
    addTags "Jefferson Airplane" "White rabbit" ["60s", "awesome"] <> apiKey ak <> sessionKey sk

  testBan = check ok . sign s $
    ban "Eminem" "Kim" <> apiKey ak <> sessionKey sk

  testLove = check ok . sign s $
    love "Gojira" "Ocean" <> apiKey ak <> sessionKey sk

  testRemoveTag = check ok . sign s $
    removeTag "Jefferson Airplane" "White rabbit" "awesome" <> apiKey ak <> sessionKey sk

  testShare = check ok . sign s $
    share "Led Zeppelin" "When the Levee Breaks" "liblastfm" <> message "Just listen!" <> apiKey ak <> sessionKey sk

  testUnban = check ok . sign s $
    unban "Eminem" "Kim" <> apiKey ak <> sessionKey sk

  testUnlove = check ok . sign s $
    unlove "Gojira" "Ocean" <> apiKey ak <> sessionKey sk

  testScrobble = check ss . sign s $
    scrobble "Gojira" "Ocean" 1300000000 <> apiKey ak <> sessionKey sk

  testUpdateNowPlaying = check np . sign s $
    updateNowPlaying "Gojira" "Ocean" <> apiKey ak <> sessionKey sk


noauth ∷ [Test]
noauth =
  [ testCase "Track.getBuylinks" testGetBuylinks
  , testCase "Track.getCorrection" testGetCorrection
  , testCase "Track.getFingerprintMetadata" testGetFingerprintMetadata
  , testCase "Track.getInfo" testGetInfo
  , testCase "Track.getShouts" testGetShouts
  , testCase "Track.getSimilar" testGetSimilar
  , testCase "Track.getTags" testGetTags
  , testCase "Track.getTopFans" testGetTopFans
  , testCase "Track.getTopTags" testGetTopTags
  , testCase "Track.search" testSearch
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetBuylinks = check gbl $
    getBuyLinks "Pink Floyd" "Brain Damage" "United Kingdom" <> apiKey ak

  testGetCorrection = check gc $
    getCorrection "Pink Ployd" "Brain Damage" <> apiKey ak

  testGetFingerprintMetadata = check gfm $
    getFingerprintMetadata 1234 <> apiKey ak

  testGetInfo = check gi $
    getInfo "Pink Floyd" "Brain Damage" <> username "aswalrus" <> apiKey ak

  testGetShouts = check gsh $
    getShouts "Pink Floyd" "Comfortably Numb" <> limit 7 <> apiKey ak

  testGetSimilar = check gsi $
    getSimilar "Pink Floyd" "Comfortably Numb" <> limit 4 <> apiKey ak

  testGetTags = check gt $
    getTags "Jefferson Airplane" "White Rabbit" "liblastfm" <> apiKey ak

  testGetTopFans = check gtf $
    getTopFans "Pink Floyd" "Comfortably Numb" <> apiKey ak

  testGetTopTags = check gtt $
    getTopTags "Pink Floyd" "Brain Damage" <> apiKey ak

  testSearch = check s' $
    search "Believe" <> limit 12 <> apiKey ak


gc, gi, gt, ss, np ∷ Value → Parser String
gbl, gfm, gsh, gsi{-, gta-}, gtf, gtt, s' ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "downloads") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gc o = parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "track") >>= (.: "artist") >>= (.: "name")
gfm o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
gi o = parseJSON o >>= (.: "track") >>= (.: "userplaycount")
gsh o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "author")
gsi o = parseJSON o >>= (.: "similartracks") >>= (.: "track") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tags") >>= (.: "@attr") >>= (.: "track")
gtf o = parseJSON o >>= (.: "topfans") >>= (.: "user") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
s' o = parseJSON o >>= (.: "results") >>= (.: "trackmatches") >>= (.: "track") >>= mapM (.: "name")
ss o = parseJSON o >>= (.: "scrobbles") >>= (.: "scrobble") >>= (.: "track") >>= (.: "#text")
np o = parseJSON o >>= (.: "nowplaying") >>= (.: "track") >>= (.: "#text")
