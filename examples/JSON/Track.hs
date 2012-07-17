{-# LANGUAGE FlexibleInstances #-}
module JSON.Track (private, public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (compare)

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
  ]
 where
  testAddTags = assert $
    addTags (Artist "Jefferson Airplane") (Track "White rabbit") [Tag "60s", Tag "awesome"] ak sk s

  testBan = assert $
    ban (Artist "Eminem") (Track "Kim") ak sk s


public ∷ [Test]
public =
  [ TestLabel "getBuylinks" $ TestCase testGetBuylinks
  , TestLabel "getCorrection" $ TestCase testGetCorrection
  , TestLabel "getFingerprintMetadata" $ TestCase testGetFingerprintMetadata
  ]
  where

  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetBuylinks = assert
    (getBuyLinks (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Country "United Kingdom") ak, decode ∷ Response → Maybe GBL)
  testGetCorrection = assert
    (getCorrection (Artist "Pink Ployd") (Track "Brain Damage") ak, decode ∷ Response → Maybe GC)
  testGetFingerprintMetadata = assert
    (getFingerprintMetadata (Fingerprint 1234) ak, decode ∷ Response → Maybe GFM)


newtype GBL = GBL [String] deriving Show
newtype GC = GC String deriving Show
newtype GFM = GFM [String] deriving Show


instance FromJSON GBL where
  parseJSON o = GBL <$> (parseJSON o >>= (.: "affiliations") >>= (.: "downloads") >>= (.: "affiliation") >>= mapM (.: "supplierName"))
instance FromJSON GC where
  parseJSON o = GC <$> (parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "track") >>= (.: "artist") >>= (.: "name"))
instance FromJSON GFM where
  parseJSON o = GFM <$> (parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name"))
