{-# LANGUAGE FlexibleInstances #-}
module JSON.Artist (private, public) where

import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Artist
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
  [ TestLabel "addTags" $ TestCase testAddTags
  , TestLabel "getTags-authenticated" $ TestCase testGetTagsAuth
  , TestLabel "removeTag" $ TestCase testRemoveTag
  , TestLabel "share" $ TestCase testShare
  ]
 where
  testGetTagsAuth = assert $
    p gt ..: getTags (Left $ Artist "Егор Летов") Nothing (Right (sk, s)) ak

  testRemoveTag = assert $
    removeTag (Artist "Егор Летов") (Tag "russian") ak sk s

  testShare = assert $
    share (Artist "Sleep") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s

  testAddTags = assert $
    addTags (Artist "Егор Летов") [Tag "russian", Tag "black metal"] ak sk s


public ∷ [Test]
public =
  [ TestLabel "getCorrection" $ TestCase testGetCorrection
  , TestLabel "getEvents" $ TestCase testGetEvents
  , TestLabel "getImages" $ TestCase testGetImages
  , TestLabel "getInfo" $ TestCase testGetInfo
  , TestLabel "getPastEvents" $ TestCase testGetPastEvents
  , TestLabel "getPodcast" $ TestCase testGetPodcast
  , TestLabel "getShouts" $ TestCase testGetShouts
  , TestLabel "getSimilar" $ TestCase testGetSimilar
  , TestLabel "getTags" $ TestCase testGetTags
  , TestLabel "getTopAlbums" $ TestCase testGetTopAlbums
  , TestLabel "getTopFans" $ TestCase testGetTopFans
  , TestLabel "getTopTags" $ TestCase testGetTopTags
  , TestLabel "getTopTracks" $ TestCase testGetTopTracks
  , TestLabel "search" $ TestCase testSearch
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetCorrection = assert $
    p gc ..: getCorrection (Artist "Meshugah") ak

  testGetEvents = assert $
    p ge ..: getEvents (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 2) Nothing ak

  testGetImages = assert $
    p gi ..: getImages (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) Nothing ak

  testGetInfo = assert $
    p gin ..: getInfo (Left $ Artist "Meshuggah") Nothing Nothing Nothing ak

  testGetPastEvents = assert $
    p gpe ..: getPastEvents (Left $ Artist "Meshugah") (Just $ Autocorrect True) Nothing Nothing ak

  testGetPodcast = assert $
    p gp ..: getPodcast (Left $ Artist "Meshuggah") Nothing ak

  testGetShouts = assert $
    p gs ..: getShouts (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 5) ak

  testGetSimilar = assert $
    p gsi ..: getSimilar (Left $ Artist "Meshuggah") Nothing (Just $ Limit 3) ak

  testGetTags = assert $
    p gt ..: getTags (Left $ Artist "Егор Летов") Nothing (Left $ User "liblastfm") ak

  testGetTopAlbums = assert $
    p gta ..: getTopAlbums (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) ak

  testGetTopFans = assert $
    p gtf ..: getTopFans (Left $ Artist "Meshuggah") Nothing ak

  testGetTopTags = assert $
    p gtt ..: getTopTags (Left $ Artist "Meshuggah") Nothing ak

  testGetTopTracks = assert $
    p gttr ..: getTopTracks (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) ak

  testSearch = assert $
    p se ..: search (Artist "Mesh") Nothing (Just $ Limit 3) ak


gc, gin, gp ∷ Value → Parser String
ge, gi, gpe, gs, gsi, gt, gta, gtf, gtt, gttr, se ∷ Value → Parser [String]
gc o = parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "artist") >>= (.: "name")
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\o' → (o' .: "venue") >>= (.: "name"))
gi o = parseJSON o >>= (.: "images") >>= (.: "image") >>= mapM (.: "url")
gin o = parseJSON o >>= (.: "artist") >>= (.: "stats") >>= (.: "listeners")
gp o = parseJSON o >>= (.: "rss") >>= (.: "channel") >>= (.: "description")
gpe o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "title")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "author")
gsi o = parseJSON o >>= (.: "similarartists") >>= (.: "artist") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gta o = parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "name")
gtf o = parseJSON o >>= (.: "topfans") >>= (.: "user") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gttr o = parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name")
se o = parseJSON o >>= (.: "results") >>= (.: "artistmatches") >>= (.: "artist") >>= mapM (.: "name")
