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

import           Control.Lens                -- lens
import           Data.Aeson.Lens             -- lens
import           Data.Traversable (for)      -- base
import           Data.Aeson (Value)          -- aeson
import           Data.Foldable (Foldable)    -- base
import           Data.Int (Int64)            -- base
import           Data.Text (Text)            -- text
import qualified Data.Text.IO as Text        -- text
import           Network.Lastfm              -- liblastfm
import qualified Network.Lastfm.User as User -- liblastfm


main :: IO ()
main = do
  r <- for [1..pages] $ \p -> parse `fmap` query (User.getRecommendedArtists <* page p)
  mapM_ Text.putStrLn (concat r)

-- Construct signed query
query :: Request JSON (APIKey -> SessionKey -> Sign) -> IO (Either LastfmError (Response JSON))
query r = lastfm $ sign secret (r <*> ak <*> sk <* json)
 where
  ak     = apiKey "__YOUR_API_KEY__"
  sk     = sessionKey "__YOUR_SESSION_KEY__"
  secret = "__YOUR_SECRET__"

-- Other query parameters
total, pages :: Int64
total = 250
pages = total `div` 50 -- 50 is the default number of recommendations per page

-- Parse artist names from response
parse :: Foldable f => f Value -> [Text]
parse x = x ^.. folded.key "recommendations".key "artist"._Array.folded.key "name"._String
