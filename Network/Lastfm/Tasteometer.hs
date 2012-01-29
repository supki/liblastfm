module Network.Lastfm.Tasteometer
  ( getScore
  , getSimilarArtists
  ) where

import Control.Monad (liftM)
import Network.Lastfm.Core

type Username = String
type APIKey = String

getScore :: APIKey -> Username -> Username -> IO (Maybe String)
getScore apiKey username1 username2 = liftM (tagContent "score") $ callAPI 
    [ ("method", "tasteometer.compare")
    , ("type1", "user"), ("value1", username1)
    , ("type2", "user"), ("value2", username2)
    , ("api_key", apiKey) ]

getSimilarArtists :: APIKey -> Username -> Username -> IO [String]
getSimilarArtists apiKey username1 username2 = liftM (init . init . tagContents "name") $ callAPI
    [ ("method", "tasteometer.compare")
    , ("type1", "user"), ("value1", username1)
    , ("type2", "user"), ("value2", username2)
    , ("api_key", apiKey) ]
