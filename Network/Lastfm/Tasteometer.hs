module Network.Lastfm.Tasteometer
  ( getScore
  , getSimilarArtists
  ) where

import Prelude hiding (compare)
import Control.Applicative ((<$>))
import Text.XML.Light
import Network.Lastfm.Core

type Username = String
type APIKey = String

compare apiKey username1 username2 = callAPI
  [ ("method", "tasteometer.compare")
  , ("type1", "user"), ("value1", username1)
  , ("type2", "user"), ("value2", username2)
  , ("limit", "10")
  , ("api_key", apiKey) ]

getScore :: APIKey -> Username -> Username -> IO (Maybe String)
getScore apiKey username1 username2 = tagContent "score" <$> compare apiKey username1 username2

getSimilarArtists :: APIKey -> Username -> Username -> IO [String]
getSimilarArtists apiKey username1 username2 = tagContentsInGroup "name" "artist" <$> compare apiKey username1 username2
    where
        tagContentsInGroup tag group = tagContents tag <$> concatMap (findElements . unqual $ group)
