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
import           Control.Lens.Aeson
import           Data.Aeson (Value)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Network.Lastfm
import qualified Network.Lastfm.User as User


main :: IO ()
main = do
  r <- for [1..pages] $ \p -> parse `fmap` query (User.getRecommendedArtists <* page p)
  mapM_ T.putStrLn (concat r)


-- | Construct signed query
query :: Request JSON (APIKey -> SessionKey -> Sign) -> IO (Response JSON)
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
parse :: Maybe Value -> [Text]
parse x = x ^.. _Just . key "recommendations" . key "artist" . _Array . folded . key "name" . _String
