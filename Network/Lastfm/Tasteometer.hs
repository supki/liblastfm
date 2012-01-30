module Network.Lastfm.Tasteometer
  ( compare
  ) where

import Prelude hiding (compare)
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Network.Lastfm.Core

type APIKey = String

data Value = User String | Artists [String] -- ^ [Last.fm username] | [Comma-separated artist names (max. 100)]
type Limit = Maybe Int                      -- ^ How many shared artists to display

instance Show Value where
  show (User user) = "user"
  show (Artists artists) = "artists"

compare :: APIKey -> Value -> Value -> Limit -> IO Response
compare apiKey value1 value2 limit = callAPI
  [ ("method", "tasteometer.compare")
  , ("type1", show value1), ("value1", getValue value1)
  , ("type2", show value2), ("value2", getValue value2)
  , ("limit", show . fromMaybe 5 $ limit)
  , ("api_key", apiKey) ]
  where
    getValue :: Value -> String
    getValue (User user) = user
    getValue (Artists artists) = intercalate "," artists

getScore :: APIKey -> Value -> Value -> IO (Maybe String)
getScore apiKey username1 username2 =
  firstInnerTagContent "score" <$> compare apiKey username1 username2 (Just 10)

getSimilarArtists :: APIKey -> Value -> Value -> IO [String]
getSimilarArtists apiKey username1 username2 =
  allInnerTagsContent "name" <$> getAllInnerTags "artist" <$> compare apiKey username1 username2 (Just 10)
