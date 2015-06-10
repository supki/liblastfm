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
import           Data.Aeson.Lens             -- lens-aeson
import           Data.Traversable (for)      -- base
import           Data.Aeson (Value)          -- aeson
import           Data.Foldable (Foldable)    -- base
import           Data.Int (Int64)            -- base
import           Data.Text (Text)            -- text
import qualified Data.Text.IO as Text        -- text
import           Network.Lastfm              -- liblastfm
import qualified Network.Lastfm.User as User -- liblastfm


main :: IO ()
main = withConnection $ \conn -> do
  r <- for [1..pages] $ \p -> parse `fmap` query conn (User.getRecommendedArtists <* page p)
  mapM_ Text.putStrLn (concat r)

-- Construct signed query
query :: Connection -> Request 'JSON (APIKey -> SessionKey -> Sign) -> IO (Either LastfmError Value)
query conn r = lastfm conn $ sign secret (r <*> ak <*> sk <* json)
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
parse x = x ^.. folded.key "recommendations".key "artist".values.key "name"._String
