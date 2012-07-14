{-# LANGUAGE FlexibleInstances #-}
module JSON.Album (private, public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Album
import Test.HUnit


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())
instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "addTags" $ TestCase testAddTags
  , TestLabel "getTags-authenticated" $ TestCase testGetTagsAuth
  , TestLabel "removeTag" $ TestCase testRemoveTag
  , TestLabel "share" $ TestCase testShare
  ]
 where
  testAddTags = assert $
    addTags (Artist "Pink Floyd", Album "The Wall") [Tag "70s", Tag "awesome", Tag "classic"] ak sk s

  testGetTagsAuth = assert
    (getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Right (sk, s)) ak, decode ∷ Response → Maybe GT)

  testRemoveTag = assert $
    removeTag (Artist "Pink Floyd") (Album "The Wall") (Tag "awesome") ak sk s

  testShare = assert $
    share (Artist "Sleep") (Album "Jerusalem") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s


public ∷ [Test]
public =
  [ TestLabel "getBuyLinks" $ TestCase testGetBuylinks
  , TestLabel "getInfo" $ TestCase testGetInfo
  , TestLabel "getShouts" $ TestCase testGetShouts
  , TestLabel "getTags" $ TestCase testGetTags
  , TestLabel "getTopTags" $ TestCase testGetTopTags
  , TestLabel "search" $ TestCase testSearch
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetBuylinks = assert
    (getBuyLinks (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Country "United Kingdom") ak, decode ∷ Response → Maybe GBL)

  testGetInfo = assert
    (getInfo (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing Nothing ak, decode ∷ Response → Maybe GI)

  testGetShouts = assert
    (getShouts (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing (Just $ Limit 7) ak, decode ∷ Response → Maybe GS)

  testGetTags = assert
    (getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Left $ User "liblastfm") ak, decode ∷ Response → Maybe GT)

  testGetTopTags = assert
    (getTopTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing ak, decode ∷ Response → Maybe GTT)

  testSearch = assert
    (search (Album "wall") Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe SE)


newtype GBL = GBL [String] deriving Show
newtype GI = GI [String] deriving Show
newtype GS = GS [String] deriving Show
newtype GT = GT [String] deriving Show
newtype GTT = GTT [String] deriving Show
newtype SE = SE [String] deriving Show


instance FromJSON GBL where
  parseJSON o = GBL <$> (parseJSON o >>= (.: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName"))
instance FromJSON GI where
  parseJSON o = GI <$> (parseJSON o >>= (.: "album") >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GS where
  parseJSON o = GS <$> (parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GTT where
  parseJSON o = GTT <$> (parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "count"))
instance FromJSON SE where
  parseJSON o = SE <$> (parseJSON o >>= (.: "results") >>= (.: "albummatches") >>= (.: "album") >>= mapM (.: "name"))
