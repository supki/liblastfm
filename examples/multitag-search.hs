#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}
{- This example shows how to do multitag search even
 - if it's not provided by Lastfm API
 -
 - Sample output:
 -
 - $> ./examples/multitag-search.hs
 - 坂本真綾
 - KOTOKO
 - Lisa
 - 茅原実里
 -}
import           Control.Applicative
import           Data.Aeson.Types
import           Data.List (intersect)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Network.Lastfm
import qualified Network.Lastfm.Tag as Tag


main :: IO ()
main = mapM_ T.putStrLn =<< artists multitags


artists :: [Text] -> IO [Text]
artists ts = do
  names <- catMaybes <$> mapM (((parseMaybe gta =<<) <$>) . query) ts
  case names of
    [] -> return []
    ns -> return (foldl1 intersect ns)
 where
  query t = lastfm $ Tag.getTopArtists <*> tag t <* limit 100 <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json

gta :: Value -> Parser [Text]
gta o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")

multitags :: [Text]
multitags = ["j-pop", "anime", "anime-ost"]
