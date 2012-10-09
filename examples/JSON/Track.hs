{-# LANGUAGE FlexibleInstances #-}
module JSON.Track (private, public) where

import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Track
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
  , TestLabel "ban" $ TestCase testBan
  , TestLabel "getTagsAuth" $ TestCase testGetTagsAuth
  , TestLabel "love" $ TestCase testLove
  , TestLabel "removeTag" $ TestCase testRemoveTag
  , TestLabel "share" $ TestCase testShare
  , TestLabel "unban" $ TestCase testUnban
  , TestLabel "unlove" $ TestCase testUnlove
  , TestLabel "scrobble" $ TestCase testScrobble
  , TestLabel "updateNowPlaying" $ TestCase testUpdateNowPlaying
  ]
 where
  testAddTags = assert $
    addTags (Artist "Jefferson Airplane") (Track "White rabbit") [Tag "60s", Tag "awesome"] ak sk s

  testBan = assert $
    ban (Artist "Eminem") (Track "Kim") ak sk s

  testGetTagsAuth = assert $
    p gta ..: getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Right (sk, s)) ak

  testLove = assert $
    love (Artist "Gojira") (Track "Ocean") ak sk s

  testRemoveTag = assert $
    removeTag (Artist "Jefferson Airplane") (Track "White rabbit") (Tag "awesome") ak sk s

  testShare = assert $
    share (Artist "Led Zeppelin") (Track "When the Levee Breaks") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s

  testUnban = assert $
    unban (Artist "Eminem") (Track "Kim") ak sk s

  testUnlove = assert $
    unlove (Artist "Gojira") (Track "Ocean") ak sk s

  testScrobble = assert $ do
    scrobble (Timestamp 130000000, Nothing, Artist "Gojira", Track "Ocean", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ak sk s

  testUpdateNowPlaying = assert $
    updateNowPlaying (Artist "Gojira") (Track "Ocean") Nothing Nothing Nothing Nothing Nothing Nothing ak sk s


public ∷ [Test]
public =
  [ TestLabel "getBuylinks" $ TestCase testGetBuylinks
  , TestLabel "getCorrection" $ TestCase testGetCorrection
  , TestLabel "getFingerprintMetadata" $ TestCase testGetFingerprintMetadata
  , TestLabel "getInfo" $ TestCase testGetInfo
  , TestLabel "getShouts" $ TestCase testGetShouts
  , TestLabel "getSimilar" $ TestCase testGetSimilar
  , TestLabel "getTags" $ TestCase testGetTags
  , TestLabel "getTopFans" $ TestCase testGetTopFans
  , TestLabel "getTopTags" $ TestCase testGetTopTags
  , TestLabel "search" $ TestCase testSearch
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetBuylinks = assert $
    p gbl ..: getBuyLinks (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Country "United Kingdom") ak

  testGetCorrection = assert $
    p gc ..: getCorrection (Artist "Pink Ployd") (Track "Brain Damage") ak

  testGetFingerprintMetadata = assert $
    p gfm ..: getFingerprintMetadata (Fingerprint 1234) ak

  testGetInfo = assert $
    p gi ..: getInfo (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Just $ Username "aswalrus") ak

  testGetShouts = assert $
    p gsh ..: getShouts (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing Nothing (Just $ Limit 7) ak

  testGetSimilar = assert $
    p gsi ..: getSimilar (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing (Just $ Limit 4) ak

  testGetTags = assert $
    p gt ..: getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Left $ User "liblastfm") ak

  testGetTopFans = assert $
    p gtf ..: getTopFans (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing ak

  testGetTopTags = assert $
    p gtt ..: getTopTags (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing ak

  testSearch = assert $
    p s' ..: search (Track "Believe") Nothing (Just $ Limit 12) Nothing ak


gc, gi, gt ∷ Value → Parser String
gbl, gfm, gsh, gsi, gta, gtf, gtt, s' ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "downloads") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gc o = parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "track") >>= (.: "artist") >>= (.: "name")
gfm o = parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name")
gi o = parseJSON o >>= (.: "track") >>= (.: "userplaycount")
gsh o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "author")
gsi o = parseJSON o >>= (.: "similartracks") >>= (.: "track") >>= mapM (.: "name")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= (.: "name")
gta o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gtf o = parseJSON o >>= (.: "topfans") >>= (.: "user") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
s' o = parseJSON o >>= (.: "results") >>= (.: "trackmatches") >>= (.: "track") >>= mapM (.: "name")
