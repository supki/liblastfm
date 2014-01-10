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
import           Data.Aeson.Types          -- aeson
import           Data.List (intersect)     -- base
import           Data.Maybe                -- base
import           Data.Text (Text)          -- text
import qualified Data.Text.IO as Text      -- text
import           Network.Lastfm            -- liblastfm
import qualified Network.Lastfm.Tag as Tag -- liblastfm

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


main :: IO ()
main = mapM_ Text.putStrLn =<< get_artists multitags


get_artists :: [Text] -> IO [Text]
get_artists ts = do
  names <- mapM request ts
  case catMaybes names of
    [] -> return []
    ns -> return (foldl1 intersect ns)
 where
  request = fmap (either (const Nothing) (parseMaybe gta)) . query

  query t = lastfm $ Tag.getTopArtists <*> tag t <* limit 100 <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json

gta :: Value -> Parser [Text]
gta o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")

multitags :: [Text]
multitags = ["j-pop", "anime", "anime-ost"]
