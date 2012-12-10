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

import Control.Applicative
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Read (readMaybe)

import           Control.Concurrent.Async
import           Control.Lens
import           Data.Aeson (Value)
import           Data.Aeson.Lens
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import           Network.Lastfm
import qualified Network.Lastfm.User as User
import qualified Network.Lastfm.Tasteometer as Tasteometer


type Score = Text


main :: IO ()
main = pages >>= scores . names >>= pretty
 where
  names = catMaybes .
    toListOf (folded . traverseArray . key "name") . toListOf (folded . key "friends" . key "user")


pages :: IO [Maybe Value]
pages = do
  first <- query (User.getFriends target)
  let ps = fromMaybe 1 (readMaybe =<< first ^. total)
  (first :) <$> forConcurrently [2..ps] (\p -> query (User.getFriends target <> page p))
 where
  total = key "friends" . key "@attr" . key "totalPages"


scores :: [User] -> IO [(User, Score)]
scores xs = zip xs <$> forConcurrently xs (\x ->
  fromMaybe "0" . view score <$> query (Tasteometer.compare target x))
 where
  score = key "comparison" . key "result" . key "score"


forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently = flip mapConcurrently


pretty :: [(User, Score)] -> IO ()
pretty = mapM_ (\(n,s) -> T.putStrLn $ n <> ": " <> s) . take 5 . sortBy (flip compare `on` snd)


query :: Request Ready JSON -> IO (Response JSON)
query r = lastfm (r <> apiKey "234fc6e0f41f6ef99b7bd62ebaf8d318" <> json)


target :: User
target = "MCDOOMDESTROYER"
