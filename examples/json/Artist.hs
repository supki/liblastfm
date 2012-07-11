{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Artist (main) where

import Control.Applicative ((<$>), empty)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Artist


main ∷ IO ()
main = common >> auth


common ∷ IO ()
common = mapM_ ($ ak)
  [ exampleGetCorrection
  , exampleGetEvents
  , exampleGetImages
  , exampleGetInfo
  , exampleGetPastEvents
  , exampleGetPodcast
  , exampleGetShouts
  , exampleGetSimilar
  , exampleGetTags
  , exampleGetTopAlbums
  , exampleGetTopFans
  , exampleGetTopTags
  , exampleGetTopTracks
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
    {- , for shout see User.shout example -}
       ]
 where
  getConfig fp =
    do (apiKey:sessionKey:secret:_) ← map (drop 1 . dropWhile (/= '=') . filter (not . isSpace)) . lines <$> readFile fp
       return (APIKey apiKey, SessionKey sessionKey, Secret secret)


exampleAddTags ∷ APIKey → SessionKey → Secret → IO ()
exampleAddTags ak sk s =
  do r ← addTags (Artist "Егор Летов") [Tag "russian", Tag "black metal"] ak sk s
     putStrLn $ case r of
       Left e → "addTags: ERROR! " <> show e
       Right _ → "addTags: OK!"


exampleGetCorrection ∷ APIKey → IO ()
exampleGetCorrection ak =
  do r ← getCorrection (Artist "Meshugah") ak
     putStrLn $ case r of
       Left e → "getCorrection: ERROR! " <> show e
       Right r' → "getCorrection: OK! Correction: " <> show (unGC <$> decode r')


exampleGetEvents ∷ APIKey → IO ()
exampleGetEvents ak =
  do r ← getEvents (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 2) Nothing ak
     putStrLn $ case r of
       Left e → "getEvents: ERROR! " <> show e
       Right r' → "getEvents: OK! 2 event places: " <> show (unGE <$> decode r')


exampleGetImages ∷ APIKey → IO ()
exampleGetImages ak =
  do r ← getImages (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) Nothing ak
     putStrLn $ case r of
       Left e → "getImages: ERROR! " <> show e
       Right r' → "getImages: OK! First 3 images links: " <> show (unGI <$> decode r')


exampleGetInfo ∷ APIKey → IO ()
exampleGetInfo ak =
  do r ← getInfo (Left $ Artist "Meshuggah") Nothing Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getInfo: ERROR! " <> show e
       Right r' → "getInfo: OK! Listeners count: " <> show (unGIN <$> decode r')


exampleGetPastEvents ∷ APIKey → IO ()
exampleGetPastEvents ak =
  do r ← getPastEvents (Left $ Artist "Meshugah") (Just $ Autocorrect True) Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getPastEvents: ERROR! " <> show e
       Right r' → "getPastEvents: OK! 3 events titles: " <> show (take 3 . unGPE <$> decode r')


exampleGetPodcast ∷ APIKey → IO ()
exampleGetPodcast ak =
  do r ← getPodcast (Left $ Artist "Meshuggah") Nothing ak
     putStrLn $ case r of
       Left e → "getPodcast: ERROR! " <> show e
       Right r' → "getPodcast: OK! First channel description: " <> show (unGP <$> decode r')


exampleGetShouts ∷ APIKey → IO ()
exampleGetShouts ak =
  do r ← getShouts (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 5) ak
     putStrLn $ case r of
       Left e → "getShouts: ERROR! " <> show e
       Right r' → "getShouts: OK! Last 5 shouts authors: " <> show (unGS <$> decode r')


exampleGetSimilar ∷ APIKey → IO ()
exampleGetSimilar ak =
  do r ← getSimilar (Left $ Artist "Meshuggah") Nothing (Just $ Limit 3) ak
     putStrLn $ case r of
       Left e → "getSimilar: ERROR! " <> show e
       Right r' → "getSimilar: OK! 3 similar artists: " <> show (unGSI <$> decode r')


exampleGetTags ∷ APIKey → IO ()
exampleGetTags ak =
  do r ← getTags (Left $ Artist "Егор Летов") Nothing (Left $ User "liblastfm") ak
     putStrLn $ case r of
       Left e → "getTags: ERROR! " <> show e
       Right r' → "getTags: OK! Егор Летов tags: " <> show (unGT <$> decode r')


exampleGetTagsAuth ∷ APIKey → SessionKey → Secret → IO ()
exampleGetTagsAuth ak sk s =
  do r ← getTags (Left $ Artist "Егор Летов") Nothing (Right (sk, s)) ak
     putStrLn $ case r of
       Left e → "getTags: ERROR! " <> show e
       Right r' → "getTags: OK! Егор Летов tags: " <> show (unGT <$> decode r')


exampleGetTopAlbums ∷ APIKey → IO ()
exampleGetTopAlbums ak =
  do r ← getTopAlbums (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) ak
     putStrLn $ case r of
       Left e → "getTopTags: ERROR! " <> show e
       Right r' → "getTopTags: OK! 3 most popular albums: " <> show (unGTA <$> decode r')


exampleGetTopFans ∷ APIKey → IO ()
exampleGetTopFans ak =
  do r ← getTopFans (Left $ Artist "Meshuggah") Nothing ak
     putStrLn $ case r of
       Left e → "getTopFans: ERROR! " <> show e
       Right r' → "getTopFans: OK! Top fans: " <> show (take 5 . unGTF <$> decode r')


exampleGetTopTags ∷ APIKey → IO ()
exampleGetTopTags ak =
  do r ← getTopTags (Left $ Artist "Meshuggah") Nothing ak
     putStrLn $ case r of
       Left e → "getTopTags: ERROR! " <> show e
       Right r' → "getTopTags: OK! Top tags: " <> show (take 5 . unGTT <$> decode r')


exampleGetTopTracks ∷ APIKey → IO ()
exampleGetTopTracks ak =
  do r ← getTopTracks (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) ak
     putStrLn $ case r of
       Left e → "getTopTags: ERROR! " <> show e
       Right r' → "getTopTags: OK! 3 most popular tracks: " <> show (unGTTR <$> decode r')


exampleRemoveTag ∷ APIKey → SessionKey → Secret → IO ()
exampleRemoveTag ak sk s =
  do r ← removeTag (Artist "Егор Летов") (Tag "russian") ak sk s
     putStrLn $ case r of
       Left e → "removeTag: ERROR! " <> show e
       Right _ → "removeTag: OK!"


exampleSearch ∷ APIKey → IO ()
exampleSearch ak =
  do r ← search (Artist "Mesh") Nothing (Just $ Limit 3) ak
     putStrLn $ case r of
       Left e → "search: ERROR! " <> show e
       Right r' → "search: OK! 3 search results for \"Mesh\" query: " <> show (unSE <$> decode r')


exampleShare ∷ APIKey → SessionKey → Secret → IO ()
exampleShare ak sk s =
  do r ← share (Artist "Sleep") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s
     putStrLn $ case r of
       Left e → "share: ERROR! " <> show e
       Right _ → "share: OK!"


newtype GC = GC { unGC ∷ String } deriving Show


instance FromJSON GC where
  parseJSON (Object o) =
    GC <$> ((o .: "corrections") >>= (.: "correction") >>= (.: "artist") >>= (.: "name"))
  parseJSON _ = empty


newtype GE = GE { unGE ∷ [String] } deriving Show


instance FromJSON GE where
  parseJSON (Object o) =
    GE <$> ((o .: "events") >>= (.: "event") >>= mapM (\o' → (o' .: "venue") >>= (.: "name")))
  parseJSON _ = empty


newtype GI = GI { unGI ∷ [String] } deriving Show


instance FromJSON GI where
  parseJSON (Object o) =
    GI <$> ((o .: "images") >>= (.: "image") >>= mapM (.: "url"))
  parseJSON _ = empty


newtype GIN = GIN { unGIN ∷ String } deriving Show


instance FromJSON GIN where
  parseJSON (Object o) =
    GIN <$> ((o .: "artist") >>= (.: "stats") >>= (.: "listeners"))
  parseJSON _ = empty


newtype GP = GP { unGP ∷ String } deriving Show


instance FromJSON GP where
  parseJSON (Object o) =
    GP <$> ((o .: "rss") >>= (.: "channel") >>= (.: "description"))
  parseJSON _ = empty


newtype GPE = GPE { unGPE ∷ [String] } deriving Show


instance FromJSON GPE where
  parseJSON (Object o) =
    GPE <$> ((o .: "events") >>= (.: "event") >>= mapM (.: "title"))
  parseJSON _ = empty


newtype GS = GS { unGS ∷ [String] } deriving Show


instance FromJSON GS where
  parseJSON (Object o) =
    GS <$> ((o .: "shouts") >>= (.: "shout") >>= mapM (.: "author"))
  parseJSON _ = empty


newtype GSI = GSI { unGSI ∷ [String] } deriving Show


instance FromJSON GSI where
  parseJSON (Object o) =
    GSI <$> ((o .: "similarartists") >>= (.: "artist") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype GT = GT { unGT ∷ [String] } deriving Show


instance FromJSON GT where
  parseJSON (Object o) =
    GT <$> ((o .: "tags") >>= (.: "tag") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype GTA = GTA { unGTA ∷ [String] } deriving Show


instance FromJSON GTA where
  parseJSON (Object o) =
    GTA <$> ((o .: "topalbums") >>= (.: "album") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype GTF = GTF { unGTF ∷ [String] } deriving Show


instance FromJSON GTF where
  parseJSON (Object o) =
    GTF <$> ((o .: "topfans") >>= (.: "user") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype GTT = GTT { unGTT ∷ [String] } deriving Show


instance FromJSON GTT where
  parseJSON (Object o) =
    GTT <$> ((o .: "toptags") >>= (.: "tag") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype GTTR = GTTR { unGTTR ∷ [String] } deriving Show


instance FromJSON GTTR where
  parseJSON (Object o) =
    GTTR <$> ((o .: "toptracks") >>= (.: "track") >>= mapM (.: "name"))
  parseJSON _ = empty


newtype SE = SE { unSE ∷ [String] } deriving Show


instance FromJSON SE where
  parseJSON (Object o) =
    SE <$> ((o .: "results") >>= (.: "artistmatches") >>= (.: "artist") >>= mapM (.: "name"))
  parseJSON _ = empty
