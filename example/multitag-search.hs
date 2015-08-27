{-# LANGUAGE OverloadedStrings #-}
{- This example shows how to do multitag search even
 - if it's not provided by Lastfm API
 -
 - Sample output:
 -
 - $> ./examples/multitag-search.hs j-pop anime anime-ost
 - 坂本真綾
 - KOTOKO
 - Lisa
 - 茅原実里
 -}
import           Data.Aeson.Types             -- aeson
import           Data.List (intersect)        -- base
import           Data.Maybe                   -- base
import           Data.Text (Text)             -- text
import qualified Data.Text as Text            -- text
import qualified Data.Text.IO as Text         -- text
import           Lastfm                       -- liblastfm
import qualified Lastfm.Tag as Tag            -- liblastfm
import           System.Environment (getArgs) -- base

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


main :: IO ()
main = mapM_ Text.putStrLn =<< withConnection . getArtists . map Text.pack =<< getArgs


getArtists :: [Text] -> Connection -> IO [Text]
getArtists ts conn = do
  names <- mapM request ts
  case catMaybes names of
    [] -> return []
    ns -> return (foldl1 intersect ns)
 where
  request = fmap (either (const Nothing) (parseMaybe gta)) . query

  query t = lastfm conn $
    Tag.getTopArtists <*> tag t <* limit 100 <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json

gta :: Value -> Parser [Text]
gta o = parseJSON o >>= (.: "topartists") >>= (.: "artist") >>= mapM (.: "name")
