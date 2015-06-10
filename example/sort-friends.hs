{-# LANGUAGE DataKinds #-}
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
import           Control.Concurrent.Async                  -- async
import           Control.Lens                              -- lens
import           Data.Aeson.Lens                           -- lens-aeson
import           Data.Aeson (Value)                        -- aeson
import           Data.Function (on)                        -- base
import           Data.List (sortBy)                        -- base
import           Data.Maybe (fromMaybe)                    -- base
import           Data.Monoid ((<>))                        -- base
import           Data.Text (Text)                          -- text
import           Data.Text.Lens (unpacked)                 -- lens
import qualified Data.Text.IO as Text                      -- text
import           Network.Lastfm hiding (to)                -- liblastfm
import qualified Network.Lastfm.User as User               -- liblastfm
import qualified Network.Lastfm.Tasteometer as Tasteometer -- liblastfm
import           Text.Read (readMaybe)                     -- base


type Score = Text


main :: IO ()
main = withConnection $ \conn ->
  pages conn >>= scores conn . names >>= pretty
 where
  names x = x ^.. folded.folded.key "friends".key "user"._Array.folded.key "name"._String

pages :: Connection -> IO [Either LastfmError Value]
pages conn = do
  first <- query conn (User.getFriends <*> user target)
  let ps = fromMaybe 1 (preview total first)
  (first :) <$> forConcurrently [2..ps] (\p -> query conn (User.getFriends <*> user target <* page p))
 where
  total = folded.key "friends".key "@attr".key "totalPages"._String.unpacked.to readMaybe.folded

scores :: Connection -> [Text] -> IO [(Text, Score)]
scores conn xs = zip xs <$> forConcurrently xs (\x -> do
  r <- query conn (Tasteometer.compare (user target) (user x))
  return (fromMaybe "0" (preview score r)))
 where
  score = folded.key "comparison".key "result".key "score"._String

forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently = flip mapConcurrently

pretty :: [(Text, Score)] -> IO ()
pretty = mapM_ (\(n,s) -> Text.putStrLn $ n <> ": " <> s) . take 5 . sortBy (flip compare `on` snd)

query :: Connection -> Request 'JSON (APIKey -> Ready) -> IO (Either LastfmError Value)
query conn r = lastfm conn (r <*> apiKey "234fc6e0f41f6ef99b7bd62ebaf8d318" <* json)

target :: Text
target = "MCDOOMDESTROYER"
