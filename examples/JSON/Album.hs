{-# LANGUAGE FlexibleInstances #-}
module JSON.Album (private, public) where

import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Album
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
  , TestLabel "removTag" $ TestCase testRemoveTag
  , TestLabel "share" $ TestCase testShare
  ]
 where
  testAddTags = assert $
    addTags (Artist "Pink Floyd", Album "The Wall") [Tag "70s", Tag "awesome", Tag "classic"] ak sk s

  testGetTagsAuth = assert $
    p gt ..: getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Right (sk, s)) ak

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
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetBuylinks = assert $
    p gbl ..: getBuyLinks (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Country "United Kingdom") ak

  testGetInfo = assert $
    p gi ..: getInfo (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing Nothing ak

  testGetShouts = assert $
    p gs ..: getShouts (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing (Just $ Limit 7) ak

  testGetTags = assert $
    p gt ..: getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Left $ User "liblastfm") ak

  testGetTopTags = assert $
    p gtt ..: getTopTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing ak

  testSearch = assert $
    p se ..: search (Album "wall") Nothing (Just $ Limit 5) ak


gbl, gi, gs, gt, gtt, se ∷ Value → Parser [String]
gbl o = parseJSON o >>= (.: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName")
gi o = parseJSON o >>= (.: "album") >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "body")
gt o = parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name")
gtt o = parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "count")
se o = parseJSON o >>= (.: "results") >>= (.: "albummatches") >>= (.: "album") >>= mapM (.: "name")
