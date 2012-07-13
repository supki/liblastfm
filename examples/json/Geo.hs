{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Geo (main) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Geo


main ∷ IO ()
main = common


common ∷ IO ()
common = mapM_ ($ ak)
  [ exampleGetEvents
  , exampleGetMetroArtistChart
  , exampleGetMetroHypeArtistChart
  , exampleGetMetroHypeTrackChart
  , exampleGetMetroTrackChart
  , exampleGetMetroUniqueArtistChart
  , exampleGetMetroUniqueTrackChart
  , exampleGetMetroWeeklyChartlist
  , exampleGetMetros
  , exampleGetTopArtists
  , exampleGetTopTracks
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"


exampleGetEvents ∷ APIKey → IO ()
exampleGetEvents ak =
  do r ← getEvents Nothing Nothing (Just $ Location "Moscow") Nothing Nothing (Just $ Limit 5) ak
     putStrLn $ case r of
       Left e → "getEvents: ERROR! " <> show e
       Right r' → "getEvents: OK! First 5 Moscow event id: " <> show (unGE <$> decode r')


exampleGetMetroArtistChart ∷ APIKey → IO ()
exampleGetMetroArtistChart ak =
  do r ← getMetroArtistChart (Country "Russia") (Metro "Saint Petersburg") Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getMetroArtistChart: ERROR! " <> show e
       Right r' → "getMetroArtistChart: OK! Top 2 Saint Petersburg artists: " <> show (take 2 . unGA <$> decode r')


exampleGetMetroHypeArtistChart ∷ APIKey → IO ()
exampleGetMetroHypeArtistChart ak =
  do r ← getMetroHypeArtistChart (Country "United States") (Metro "New York") Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getMetroHypeArtistChart: ERROR! " <> show e
       Right r' → "getMetroHypeArtistChart: OK! Top 2 New York Hype artists: " <> show (take 2 . unGA <$> decode r')


exampleGetMetroHypeTrackChart ∷ APIKey → IO ()
exampleGetMetroHypeTrackChart ak =
  do r ← getMetroHypeTrackChart (Country "Russia") (Metro "Ufa") Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getMetroHypeTrackChart: ERROR! " <> show e
       Right r' → "getMetroHypeTrackChart: OK! Top 3 Ufa Hype tracks: " <> show (take 3 . unGT <$> decode r')


exampleGetMetroTrackChart ∷ APIKey → IO ()
exampleGetMetroTrackChart ak =
  do r ← getMetroTrackChart (Country "United States") (Metro "Boston") Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getMetroTrackChart: ERROR! " <> show e
       Right r' → "getMetroTrackChart: OK! Top 2 Boston tracks: " <> show (take 2 . unGT <$> decode r')


exampleGetMetroUniqueArtistChart ∷ APIKey → IO ()
exampleGetMetroUniqueArtistChart ak =
  do r ← getMetroUniqueArtistChart (Country "Belarus") (Metro "Minsk") Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getMetroUniqueArtistChart: ERROR! " <> show e
       Right r' → "getMetroUniqueArtistChart: OK! Top 2 unique Minsk artists: " <> show (take 2 . unGA <$> decode r')


exampleGetMetroUniqueTrackChart ∷ APIKey → IO ()
exampleGetMetroUniqueTrackChart ak =
  do r ← getMetroUniqueTrackChart (Country "Russia") (Metro "Moscow") Nothing Nothing ak
     putStrLn $ case r of
       Left e → "getMetroUniqueTrackChart: ERROR! " <> show e
       Right r' → "getMetroUniqueTrackChart: OK! Top 3 unique Moscow tracks: " <> show (take 3 . unGT <$> decode r')


exampleGetMetroWeeklyChartlist ∷ APIKey → IO ()
exampleGetMetroWeeklyChartlist ak =
  do r ← getMetroWeeklyChartlist (Metro "Moscow") ak
     putStrLn $ case r of
       Left e → "getMetros: ERROR! " <> show e
       Right r' → "getMetros: OK! First 2 Moscow chartlist intervals: " <> show (take 2 . unGC <$> decode r')

exampleGetMetros ∷ APIKey → IO ()
exampleGetMetros ak =
  do r ← getMetros (Just $ Country "Russia") ak
     putStrLn $ case r of
       Left e → "getMetros: ERROR! " <> show e
       Right r' → "getMetros: OK! All Russia metros: " <> show (unGM <$> decode r')


exampleGetTopArtists ∷ APIKey → IO ()
exampleGetTopArtists ak =
  do r ← getTopArtists (Country "Belarus") Nothing (Just $ Limit 3) ak
     putStrLn $ case r of
       Left e → "getTopArtists: ERROR! " <> show e
       Right r' → "getTopArtists: OK! Top 3 Belarus artists: " <> show (unGA <$> decode r')


exampleGetTopTracks ∷ APIKey → IO ()
exampleGetTopTracks ak =
  do r ← getTopTracks (Country "Ukraine") Nothing Nothing (Just $ Limit 2) ak
     putStrLn $ case r of
       Left e → "getTopTracks: ERROR! " <> show e
       Right r' → "getTopTracks: OK! Top 2 Ukraine tracks: " <> show (unGT <$> decode r')


newtype GA = GA { unGA ∷ [String] } deriving Show
newtype GC = GC { unGC ∷ [(String,String)] } deriving Show
newtype GE = GE { unGE ∷ [String] } deriving Show
newtype GM = GM { unGM ∷ [String] } deriving Show
newtype GT = GT { unGT ∷ [String] } deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name"))
instance FromJSON GC where
  parseJSON o = GC <$> (parseJSON o >>= (.: "weeklychartlist") >>= (.: "chart") >>= mapM (\t → (,) <$> (t .: "from") <*> (t .: "to")))
instance FromJSON GE where
  parseJSON o = GE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (.: "id"))
instance FromJSON GM where
  parseJSON o = GM <$> (parseJSON o >>= (.: "metros") >>= (.: "metro") >>= mapM (.: "name"))
instance FromJSON GT where
  parseJSON o = GT <$> (parseJSON o >>= (.: "toptracks") >>= (.: "track") >>= mapM (.: "name"))
