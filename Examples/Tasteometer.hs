import Network.Lastfm.Core
import Network.Lastfm.Tasteometer
import Control.Applicative ((<$>))
import Prelude hiding (compare)

getScore :: APIKey -> Value -> Value -> IO (Maybe String)
getScore apiKey username1 username2 =
  firstInnerTagContent "score" <$> compare apiKey username1 username2 (Just 10)

getSimilarArtists :: APIKey -> Value -> Value -> IO [String]
getSimilarArtists apiKey username1 username2 =
  allInnerTagsContent "name" <$> getAllInnerTags "artist" <$> compare apiKey username1 username2 (Just 10)
