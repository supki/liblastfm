{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Album (main) where

import Control.Applicative ((<$>), empty)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Album


main ∷ IO ()
main = common >> auth


common ∷ IO ()
common = mapM_ ($ ak)
  [ exampleGetBuylinks
  , exampleGetInfo
  , exampleGetShouts
  , exampleGetTags
  , exampleGetTopTags
  , exampleSearch
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"


auth ∷ IO ()
auth =
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


exampleGetBuylinks ∷ APIKey → IO ()
exampleGetBuylinks ak =
  do r ← getBuyLinks (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Country "United Kingdom") ak
     putStrLn $ case r of
       Left e → "getBuyLinks: ERROR! " <> show e
       Right r' → "getBuyLinks: OK! Download suppliers: " <>
                    show (unGBL <$> decode r')


exampleGetInfo ∷ APIKey → IO ()
exampleGetInfo ak =
  do r ← getInfo (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getInfo: ERROR! " <> show e
       Right r' → "getInfo: OK! Top 5 tags: " <> show (unGI <$> decode r')


exampleGetShouts ∷ APIKey → IO ()
exampleGetShouts ak =
  do r ← getShouts (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing (Just $ Limit 7) ak
     putStrLn $ case r of
       Left e → "getShouts: ERROR! " <> show e
       Right r' → "getShouts: OK! Last 7 shouts: " <> show (unGS <$> decode r')


exampleGetTags ∷ APIKey → IO ()
exampleGetTags ak =
  do r ← getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Left $ User "liblastfm") ak
     putStrLn $ case r of
       Left e → "getTags: ERROR! " <> show e
       Right r' → "getTags: OK! The Wall tags: " <> show (unGT <$> decode r')


exampleGetTagsAuth ∷ APIKey → SessionKey → Secret → IO ()
exampleGetTagsAuth ak sk s =
  do r ← getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Right (sk, s)) ak
     putStrLn $ case r of
       Left e → "getTags: ERROR! " <> show e
       Right r' → "getTags: OK! The Wall tags: " <> show (unGT <$> decode r')


exampleGetTopTags ∷ APIKey → IO ()
exampleGetTopTags ak =
  do r ← getTopTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing ak
     putStrLn $ case r of
       Left e → "getTopTags: ERROR! " <> show e
       Right r' → "getTopTags: OK! Top tags counts: " <> show (take 5 . unGTT <$> decode r')


exampleRemoveTag ∷ APIKey → SessionKey → Secret → IO ()
exampleRemoveTag ak sk s =
  do r ← removeTag (Artist "Pink Floyd") (Album "The Wall") (Tag "awesome") ak sk s
     putStrLn $ case r of
       Left e → "removeTag: ERROR! " <> show e
       Right _ → "removeTag: OK!"


exampleSearch ∷ APIKey → IO ()
exampleSearch ak =
  do r ← search (Album "wall") Nothing (Just $ Limit 5) ak
     putStrLn $ case r of
       Left e → "search: ERROR! " <> show e
       Right r' → "search: OK! 5 search results for \"wall\" query: " <> show (unSE <$> decode r')


exampleShare ∷ APIKey → SessionKey → Secret → IO ()
exampleShare ak sk s =
  do r ← share (Artist "Sleep") (Album "Jerusalem") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s
     putStrLn $ case r of
       Left e → "share: ERROR! " <> show e
       Right _ → "share: OK!"


newtype GBL = GBL { unGBL ∷ [String] } deriving Show


instance FromJSON GBL where
  parseJSON (Object o) =
    GBL <$> ((o .: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName"))
  parseJSON _ = empty


newtype GI = GI { unGI ∷ [String] } deriving Show


instance FromJSON GI where
  parseJSON (Object o) =
    GI <$> ((o .: "album") >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype GS = GS { unGS ∷ [String] } deriving Show


instance FromJSON GS where
  parseJSON (Object o) =
    GS <$> ((o .: "shouts") >>= (.: "shout") >>= mapM (.: "body"))
  parseJSON _ = empty


newtype GT = GT { unGT ∷ [String] } deriving Show


instance FromJSON GT where
  parseJSON (Object o) =
    GT <$> ((o .: "tags") >>= (.: "tag") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype GTT = GTT { unGTT ∷ [String] } deriving Show


instance FromJSON GTT where
  parseJSON (Object o) =
    GTT <$> ((o .: "toptags") >>= (.: "tag") >>= mapM (.: "count"))
  parseJSON _ = empty


newtype SE = SE { unSE ∷ [String] } deriving Show


instance FromJSON SE where
  parseJSON (Object o) =
    SE <$> ((o .: "results") >>= (.: "albummatches") >>= (.: "album") >>= mapM (.: "name"))
  parseJSON _ = empty
