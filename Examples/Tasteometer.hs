import Network.Lastfm.Auth
import Network.Lastfm.Core
import Network.Lastfm.Tasteometer

import Control.Applicative ((<$>))
import Data.Maybe
import Prelude hiding (compare)

getScore :: Value -> Value -> APIKey -> IO (Maybe String)
getScore username1 username2 apiKey =
  firstInnerTagContent "score" <$> compare username1 username2 (Just . Limit $ 10) apiKey

getSimilarArtists :: Value -> Value -> APIKey -> IO [String]
getSimilarArtists username1 username2 apiKey =
  allInnerTagsContent "name" <$> getAllInnerTags "artist" <$> compare username1 username2 (Just . Limit $ 10) apiKey

main = do
  let apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
  let username1 = "smpcln"
  let user1 = ValueUser . User $ username1
  let username2 = "ingolfr"
  let user2 = ValueUser . User $ username2
  putStrLn "Score: "
  getScore user1 user2 apiKey >>= putStrLn . fromJust
  putStrLn "Similar groups: "
  getSimilarArtists user1 user2 apiKey >>= mapM_ putStrLn
