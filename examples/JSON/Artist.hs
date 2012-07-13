{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module JSON.Artist (tests) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Artist
import Test.HUnit


main ∷ IO ()
main =
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


exampleGetTagsAuth ∷ APIKey → SessionKey → Secret → IO ()
exampleGetTagsAuth ak sk s =
  do r ← getTags (Left $ Artist "Егор Летов") Nothing (Right (sk, s)) ak
     putStrLn $ case r of
       Left e → "getTags: ERROR! " <> show e
       Right r' → "getTags: OK! Егор Летов tags: " <> show (unGT <$> decode r')


exampleRemoveTag ∷ APIKey → SessionKey → Secret → IO ()
exampleRemoveTag ak sk s =
  do r ← removeTag (Artist "Егор Летов") (Tag "russian") ak sk s
     putStrLn $ case r of
       Left e → "removeTag: ERROR! " <> show e
       Right _ → "removeTag: OK!"


exampleShare ∷ APIKey → SessionKey → Secret → IO ()
exampleShare ak sk s =
  do r ← share (Artist "Sleep") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s
     putStrLn $ case r of
       Left e → "share: ERROR! " <> show e
       Right _ → "share: OK!"


exampleAddTags ∷ APIKey → SessionKey → Secret → IO ()
exampleAddTags ak sk s =
  do r ← addTags (Artist "Егор Летов") [Tag "russian", Tag "black metal"] ak sk s
     putStrLn $ case r of
       Left e → "addTags: ERROR! " <> show e
       Right _ → "addTags: OK!"


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


tests ∷ [Test]
tests =
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
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetCorrection = assert
    (getCorrection (Artist "Meshugah") ak, decode ∷ Response → Maybe GC)

  testGetEvents = assert
    (getEvents (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 2) Nothing ak, decode ∷ Response → Maybe GE)

  testGetImages = assert
    (getImages (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) Nothing ak, decode ∷ Response → Maybe GI)

  testGetInfo = assert
    (getInfo (Left $ Artist "Meshuggah") Nothing Nothing Nothing ak,  decode ∷ Response → Maybe GIN)

  testGetPastEvents = assert
    (getPastEvents (Left $ Artist "Meshugah") (Just $ Autocorrect True) Nothing Nothing ak, decode ∷ Response → Maybe GPE)

  testGetPodcast = assert
    (getPodcast (Left $ Artist "Meshuggah") Nothing ak, decode ∷ Response → Maybe GP)

  testGetShouts = assert
    (getShouts (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 5) ak, decode ∷ Response → Maybe GS)

  testGetSimilar = assert
    (getSimilar (Left $ Artist "Meshuggah") Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GSI)

  testGetTags = assert
    (getTags (Left $ Artist "Егор Летов") Nothing (Left $ User "liblastfm") ak, decode ∷ Response → Maybe GT)

  testGetTopAlbums = assert
    (getTopAlbums (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GTA)

  testGetTopFans = assert
    (getTopFans (Left $ Artist "Meshuggah") Nothing ak, decode ∷ Response → Maybe GTF)

  testGetTopTags = assert
    (getTopTags (Left $ Artist "Meshuggah") Nothing ak, decode ∷ Response → Maybe GTT)

  testGetTopTracks = assert
    (getTopTracks (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe GTTR)

  testSearch = assert
    (search (Artist "Mesh") Nothing (Just $ Limit 3) ak, decode ∷ Response → Maybe SE)


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
