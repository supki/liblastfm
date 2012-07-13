{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Chart (main) where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Chart


main ∷ IO ()
main = common


common ∷ IO ()
common = mapM_ ($ ak)
  [ exampleGetHypedArtists
  , exampleGetHypedTracks
  , exampleGetLovedTracks
  , exampleGetTopArtists
  , exampleGetTopTags
  , exampleGetTopTracks
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"


exampleGetHypedArtists ∷ APIKey → IO ()
exampleGetHypedArtists ak =
  do r ← getHypedArtists Nothing (Just $ Limit 3) ak
     putStrLn $ case r of
       Left e → "getHypedArtists: ERROR! " <> show e
       Right r' → "getHypedArtists: OK! Top 3 hyped artists: " <> show (unGA <$> decode r')


exampleGetHypedTracks ∷ APIKey → IO ()
exampleGetHypedTracks ak =
  do r ← getHypedTracks Nothing (Just $ Limit 2) ak
     putStrLn $ case r of
       Left e → "getHypedTracks: ERROR! " <> show e
       Right r' → "getHypedTracks: OK! Top 2 hyped tracks: " <> show (unGT <$> decode r')


exampleGetLovedTracks ∷ APIKey → IO ()
exampleGetLovedTracks ak =
  do r ← getLovedTracks Nothing (Just $ Limit 3) ak
     putStrLn $ case r of
       Left e → "getLovedTracks: ERROR! " <> show e
       Right r' → "getLovedTracks: OK! Top 3 most loved tracks: " <> show (unGT <$> decode r')


exampleGetTopArtists ∷ APIKey → IO ()
exampleGetTopArtists ak =
  do r ← getTopArtists Nothing (Just $ Limit 4) ak
     putStrLn $ case r of
       Left e → "getTopArtists: ERROR! " <> show e
       Right r' → "getTopArtists: OK! Top 4 artists: " <> show (unGA <$> decode r')


exampleGetTopTags ∷ APIKey → IO ()
exampleGetTopTags ak =
  do r ← getTopTags Nothing (Just $ Limit 5) ak
     putStrLn $ case r of
       Left e → "getTopTags: ERROR! " <> show e
       Right r' → "getTopTags: OK! Top 5 tags: " <> show (unGTA <$> decode r')


exampleGetTopTracks ∷ APIKey → IO ()
exampleGetTopTracks ak =
  do r ← getTopTracks Nothing (Just $ Limit 2) ak
     putStrLn $ case r of
       Left e → "getTopTracks: ERROR! " <> show e
       Right r' → "getTopTracks: OK! Top 2 tracks: " <> show (unGT <$> decode r')


newtype GA = GA { unGA ∷ [String] } deriving Show
newtype GT = GT { unGT ∷ [String] } deriving Show
newtype GTA = GTA { unGTA ∷ [String] } deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "artists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "tracks") >>= (.: "track") >>= mapM (.: "name"))
instance FromJSON GTA where
  parseJSON o = GTA <$> (parseJSON o >>= (.: "tags") >>= (.: "tag") >>= mapM (.: "name"))
