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
  getScore (ValueUser . User $ "smpcln") (ValueUser . User $ "ingolfr") (APIKey "b25b959554ed76058ac220b7b2e0a026") >>= putStrLn . fromJust
  getSimilarArtists (ValueUser . User $ "smpcln") (ValueUser . User $ "ingolfr") (APIKey "b25b959554ed76058ac220b7b2e0a026") >>= mapM_ putStrLn
