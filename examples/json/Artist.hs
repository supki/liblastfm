{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Artist (main) where

import Control.Applicative ((<$>))
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
newtype GE = GE { unGE ∷ [String] } deriving Show
newtype GI = GI { unGI ∷ [String] } deriving Show
newtype GIN = GIN { unGIN ∷ String } deriving Show
newtype GP = GP { unGP ∷ String } deriving Show
newtype GPE = GPE { unGPE ∷ [String] } deriving Show
newtype GS = GS { unGS ∷ [String] } deriving Show
newtype GSI = GSI { unGSI ∷ [String] } deriving Show
newtype GT = GT { unGT ∷ [String] } deriving Show
newtype GTA = GTA { unGTA ∷ [String] } deriving Show
newtype GTF = GTF { unGTF ∷ [String] } deriving Show
newtype GTT = GTT { unGTT ∷ [String] } deriving Show
newtype GTTR = GTTR { unGTTR ∷ [String] } deriving Show
newtype SE = SE { unSE ∷ [String] } deriving Show


instance FromJSON GC where
  parseJSON o = GC <$> (parseJSON o >>= (.: "corrections") >>= (.: "correction") >>= (.: "artist") >>= (.: "name"))
instance FromJSON GE where
  parseJSON o = GE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\o' → (o' .: "venue") >>= (.: "name")))
instance FromJSON GI where
  parseJSON o = GI <$> (parseJSON o >>= (.: "images") >>= (.: "image") >>= mapM (.: "url"))
instance FromJSON GIN where
  parseJSON o = GIN <$> (parseJSON o >>= (.: "artist") >>= (.: "stats") >>= (.: "listeners"))
instance FromJSON GP where
  parseJSON o = GP <$> (parseJSON o >>= (.: "rss") >>= (.: "channel") >>= (.: "description"))
instance FromJSON GPE where
  parseJSON o = GPE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "title"))
instance FromJSON GS where
  parseJSON o = GS <$> (parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= mapM (.: "author"))
instance FromJSON GSI where
  parseJSON o = GSI <$> (parseJSON o >>= (.: "similarartists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GTA where
  parseJSON o = GTA <$> (parseJSON o >>= (.: "topalbums") >>= (.: "album") >>= mapM (.: "name"))
instance FromJSON GTF where
  parseJSON o = GTF <$> (parseJSON o >>= (.: "topfans") >>= (.: "user") >>= mapM (.: "name"))
instance FromJSON GTT where
  parseJSON o = GTT <$> (parseJSON o >>= (.: "toptags") >>= (.: "tag") >>= mapM (.: "name"))
instance FromJSON GTTR where
  parseJSON o = GTTR <$> (parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON SE where
  parseJSON o = SE <$> (parseJSON o >>= (.: "results") >>= (.: "artistmatches") >>= (.: "artist") >>= mapM (.: "name"))
