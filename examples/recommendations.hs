{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{- This example shows how to use signed queries
 - to get user recommendations
 -
 - Sample output:
 -
 - Roger Rotor
 - Neurosis
 - Asche
 - SKYHARBOR
 - Cylob
 - Mouth of the Architect
 -}
module Main where

import           Data.Int (Int64)
import           Data.Traversable (for)

import           Control.Lens
import           Data.Aeson (Value)
import qualified Data.Aeson.Lens as L
import           Network.Lastfm
import qualified Network.Lastfm.User as User


main :: IO ()
main = do
  r <- for [1..pages] $ \p -> parse `fmap` query (User.getRecommendedArtists <* page p)
  mapM_ putStrLn (concat r)


-- | Construct signed query
query :: Request JSON Sign (APIKey -> SessionKey -> Ready) -> IO (Response JSON)
query r = lastfm $ sign secret (r <*> ak <*> sk <* json)
 where
  ak     = apiKey "__YOUR_API_KEY__"
  sk     = sessionKey "__YOUR_SESSION_KEY__"
  secret = "__YOUR_SECRET__"


-- | Other query parameters
total, pages :: Int64
total = 250
pages = total `div` 50 -- 50 is the default number of recommendations per page


-- | Parse artist names from response
parse :: Maybe Value -> [String]
parse x = x ^. L.key "recommendations" . L.key "artist" ^.. L.traverseArray . L.key "name" . folded
