module Network.Lastfm.Tasteometer
  ( compare
  , Value (..), APIKey
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Network.Lastfm.Core
import Prelude hiding (compare)

type APIKey = String

data Value = User String      -- ^ [Last.fm username]
           | Group String     -- ^ [Last.fm group name]
           | Artists [String] -- ^ [Artist names (max. 100)]
type Limit = Int              -- ^ How many shared artists to display

instance Show Value where
  show (User _)    = "user"
  show (Artists _) = "artists"
  show (Group _)   = "group"

compare :: APIKey -> Value -> Value -> Maybe Limit -> IO Response
compare apiKey value1 value2 limit = callAPI
  [ ("method", "tasteometer.compare")
  , ("type1", show value1), ("value1", getValue value1)
  , ("type2", show value2), ("value2", getValue value2)
  , ("limit", show . fromMaybe 5 $ limit)
  , ("api_key", apiKey) ]
  where
    getValue :: Value -> String
    getValue (User user) = user
    getValue (Group _) = error "cannot compare Group value"
    getValue (Artists artists) = intercalate "," artists

{- `compareGroup' method is deprecated currently -}
