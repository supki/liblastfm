import Network.Lastfm.Core (allInnerTagsContent, getAllInnerTags, firstInnerTagContent)
import Network.Lastfm.Tasteometer
import Network.Lastfm.Types (APIKey(..), Limit(..), User(..), Value (..))

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Prelude hiding (compare)

getScore :: Value -> Value -> APIKey -> IO ()
getScore username1 username2 apiKey = do
  response <- compare username1 username2 (Just . Limit $ 10) apiKey
  case response of
    Left e -> print e
    Right r -> putStrLn . fromJust . firstInnerTagContent "score" $ r

getSimilarArtists :: Value -> Value -> APIKey -> IO ()
getSimilarArtists username1 username2 apiKey = do
  response <- compare username1 username2 (Just . Limit $ 10) apiKey
  case response of
    Left e -> print e
    Right r -> mapM_ putStrLn . allInnerTagsContent "name" . getAllInnerTags "artist" $ r

main = do
  let apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
  let username1 = "smpcln"
  let user1 = ValueUser . User $ username1
  let username2 = "ingolfr"
  let user2 = ValueUser . User $ username2
  putStrLn "Score: "
  getScore user1 user2 apiKey
  putStrLn "Similar groups: "
  getSimilarArtists user1 user2 apiKey
