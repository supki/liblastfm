module Network.Lastfm.Tasteometer
  ( compare
  ) where

import Prelude hiding (compare)
import Control.Applicative ((<$>))
import Data.List (intersperse)
import Text.XML.Light
import Network.Lastfm.Core

type APIKey = String
type Response = [Element]

type Type = String                          -- ^ "user" | "artists"
data Value = User String | Artists [String] -- ^ [Last.fm username] | [Comma-separated artist names (max. 100)]
type Limit = Double                         -- ^ How many shared artists to display

compare :: APIKey -> Type -> Value -> Type -> Value -> Limit -> IO Response
compare apiKey type1 value1 type2 value2 limit = callAPI
  [ ("method", "tasteometer.compare")
  , ("type1", type1), ("value1", handleValue value1)
  , ("type2", type2), ("value2", handleValue value2)
  , ("limit", show limit)
  , ("api_key", apiKey) ]
    where
        handleValue :: Value -> String
        handleValue (User user) = user
        handleValue (Artists artists) = concat . intersperse "," $ artists

getScore :: APIKey -> Value -> Value -> IO (Maybe String)
getScore apiKey username1 username2 =
    firstInnerTagContent "score" <$> compare apiKey "user" username1 "user" username2 10

getSimilarArtists :: APIKey -> Value -> Value -> IO [String]
getSimilarArtists apiKey username1 username2 =
    allInnerTagsContent "name" <$> getAllInnerTags "artist" <$> compare apiKey "user" username1 "user" username2 10
