{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{- This example shows how to extract all
 - user friends, get their similarity score and
 - then sort them based on it outputting top 5
 -
 - User friends of which are searched is 'target'
 -
 - Most of hard work there is on 'aeson-lens' actually :)
 -
 - Sample output:
 -
 - % ./examples/sort-friends.hs
 - Artvart: 0.9969767332077
 - smpcln: 0.9965825676918
 - nv0id: 0.86804795265198
 - QueenrXy: 0.5932799577713
 - GhostOsaka: 0.2875879406929
 -
 - Notice: you may want to adjust maximum open files limit
 - if testing on users with relatively large friends count
 -}
module Main where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Text.Read (readMaybe)

import           Control.Concurrent.Async
import           Control.Lens
import           Control.Lens.Aeson
import           Data.Aeson (Value)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.Lastfm hiding (to)
import qualified Network.Lastfm.User as User
import qualified Network.Lastfm.Tasteometer as Tasteometer


type Score = Text


main :: IO ()
main = pages >>= scores . names >>= pretty
 where
  names x = x ^.. folded . _Just . key "friends" . key "user" . _Array . folded . key "name" . _String


pages :: IO [Maybe Value]
pages = do
  first <- query (User.getFriends <*> user target)
  let ps = fromMaybe 1 (readMaybe =<< preview total first)
  (first :) <$> forConcurrently [2..ps] (\p -> query (User.getFriends <*> user target <* page p))
 where
  total = _Just . key "friends" . key "@attr" . key "totalPages" . _String . to T.unpack


scores :: [Text] -> IO [(Text, Score)]
scores xs = zip xs <$> forConcurrently xs (\x -> do
  r <- query (Tasteometer.compare (user target) (user x))
  return (fromMaybe "0" (preview score r)))
 where
  score = _Just . key "comparison" . key "result" . key "score" . _String


forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently = flip mapConcurrently


pretty :: [(Text, Score)] -> IO ()
pretty = mapM_ (\(n,s) -> T.putStrLn $ n <> ": " <> s) . take 5 . sortBy (flip compare `on` snd)


query :: Request JSON (APIKey -> Ready) -> IO (Response JSON)
query r = lastfm (r <*> apiKey "234fc6e0f41f6ef99b7bd62ebaf8d318" <* json)


target :: Text
target = "MCDOOMDESTROYER"
