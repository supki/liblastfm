{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module JSON.Album (tests) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Album
import Test.HUnit


main ∷ IO ()
main =
  do (ak, sk, s) ← getConfig "../.lastfm.conf"
     mapM_ (\f → f ak sk s)
       [ exampleAddTags
       , exampleGetTagsAuth
       , exampleRemoveTag
       , exampleShare
       ]
 where
  getConfig fp =
    do (apiKey:sessionKey:secret:_) ← map (drop 1 . dropWhile (/= '=') . filter (not . isSpace)) . lines <$> readFile fp
       return (APIKey apiKey, SessionKey sessionKey, Secret secret)


exampleAddTags ∷ APIKey → SessionKey → Secret → IO ()
exampleAddTags ak sk s =
  do r ← addTags (Artist "Pink Floyd", Album "The Wall") [Tag "70s", Tag "awesome", Tag "classic"] ak sk s
     putStrLn $ case r of
       Left e → "addTags: ERROR! " <> show e
       Right _ → "addTags: OK!"


exampleGetTagsAuth ∷ APIKey → SessionKey → Secret → IO ()
exampleGetTagsAuth ak sk s =
  do r ← getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Right (sk, s)) ak
     putStrLn $ case r of
       Left e → "getTags: ERROR! " <> show e
       Right r' → "getTags: OK! The Wall tags: " <> show (unGT <$> decode r')


exampleRemoveTag ∷ APIKey → SessionKey → Secret → IO ()
exampleRemoveTag ak sk s =
  do r ← removeTag (Artist "Pink Floyd") (Album "The Wall") (Tag "awesome") ak sk s
     putStrLn $ case r of
       Left e → "removeTag: ERROR! " <> show e
       Right _ → "removeTag: OK!"


exampleShare ∷ APIKey → SessionKey → Secret → IO ()
exampleShare ak sk s =
  do r ← share (Artist "Sleep") (Album "Jerusalem") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s
     putStrLn $ case r of
       Left e → "share: ERROR! " <> show e
       Right _ → "share: OK!"


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


tests ∷ [Test]
tests =
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


newtype GBL = GBL { unGBL ∷ [String] } deriving Show
newtype GI = GI { unGI ∷ [String] } deriving Show
newtype GS = GS { unGS ∷ [String] } deriving Show
newtype GT = GT { unGT ∷ [String] } deriving Show
newtype GTT = GTT { unGTT ∷ [String] } deriving Show
newtype SE = SE { unSE ∷ [String] } deriving Show


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
