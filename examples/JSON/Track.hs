{-# LANGUAGE FlexibleInstances #-}
module JSON.Track (private, public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Track
import Test.HUnit


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())
instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


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

  testGetTagsAuth = assert
    (getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Right (sk, s)) ak, decode ∷ Response → Maybe GTA)

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

  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetBuylinks = assert
    (getBuyLinks (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Country "United Kingdom") ak, decode ∷ Response → Maybe GBL)

  testGetCorrection = assert
    (getCorrection (Artist "Pink Ployd") (Track "Brain Damage") ak, decode ∷ Response → Maybe GC)

  testGetFingerprintMetadata = assert
    (getFingerprintMetadata (Fingerprint 1234) ak, decode ∷ Response → Maybe GFM)

  testGetInfo = assert
    (getInfo (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Just $ Username "aswalrus") ak, decode ∷ Response → Maybe GI)

  testGetShouts = assert
    (getShouts (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing Nothing (Just $ Limit 7) ak, decode ∷ Response → Maybe GSH)

  testGetSimilar = assert
    (getSimilar (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing (Just $ Limit 4) ak, decode ∷ Response → Maybe GSI)

  testGetTags = assert
    (getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Left $ User "liblastfm") ak, decode ∷ Response → Maybe GT)

  testGetTopFans = assert
    (getTopFans (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing ak, decode ∷ Response → Maybe GTF)

  testGetTopTags = assert
    (getTopTags (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing ak, decode ∷ Response → Maybe GTT)

  testSearch = assert
    (search (Track "Believe") Nothing (Just $ Limit 12) Nothing ak, decode ∷ Response → Maybe S)


newtype GBL = GBL [String] deriving Show
newtype GC = GC String deriving Show
newtype GFM = GFM [String] deriving Show
newtype GI = GI String deriving Show
newtype GSH = GSH [String] deriving Show
newtype GSI = GSI [String] deriving Show
newtype GT = GT String deriving Show
newtype GTA = GTA [String] deriving Show
newtype GTF = GTF [String] deriving Show
newtype GTT = GTT [String] deriving Show
newtype S = S [String] deriving Show


instance FromJSON GBL where
  parseJSON o = GBL <$> (parseJSON o >>= (.: "affiliations") >>= (.: "downloads") >>= (.: "affiliation") >>= mapM (.: "supplierName"))
instance FromJSON GC where
  parseJSON o = GC <$> (parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "track") >>= (.: "artist") >>= (.: "name"))
instance FromJSON GFM where
  parseJSON o = GFM <$> (parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GI where
  parseJSON o = GI <$> (parseJSON o >>= (.: "track") >>= (.: "userplaycount"))
instance FromJSON GSH where
  parseJSON o = GSH <$> (parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "author"))
instance FromJSON GSI where
  parseJSON o = GSI <$> (parseJSON o >>= (.: "similartracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "tags") >>= (.: "tag") >>= (.: "name"))
instance FromJSON GTA where
  parseJSON o = GTA <$> (parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GTF where
  parseJSON o = GTF <$> (parseJSON o >>= (.: "topfans") >>= (.: "user") >>= mapM (.: "name"))
instance FromJSON GTT where
  parseJSON o = GTT <$> (parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON S where
  parseJSON o = S <$> (parseJSON o >>= (.: "results") >>= (.: "trackmatches") >>= (.: "track") >>= mapM (.: "name"))
