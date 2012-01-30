module Network.Lastfm.Tasteometer
  ( getScore
  , getSimilarArtists
  ) where

import Control.Applicative ((<$>))
import Network.Lastfm.Core

type Username = String
type APIKey = String

tasteometerCompare apiKey username1 username2 = callAPI
  [ ("method", "tasteometer.compare")
  , ("type1", "user"), ("value1", username1)
  , ("type2", "user"), ("value2", username2)
  , ("limit", "10")
  , ("api_key", apiKey) ]

getScore :: APIKey -> Username -> Username -> IO (Maybe String)
getScore apiKey username1 username2 = tagContent "score" <$> tasteometerCompare apiKey username1 username2

getSimilarArtists :: APIKey -> Username -> Username -> IO [String]
getSimilarArtists apiKey username1 username2 = (init . init . tagContents "name") <$> tasteometerCompare apiKey username1 username2
