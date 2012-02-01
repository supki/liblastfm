{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Tasteometer
  ( compare
  , Value(..), Limit(..), User(..)
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Network.Lastfm.Artist
import Network.Lastfm.Auth (APIKey)
import Network.Lastfm.Core
import Network.Lastfm.User
import Prelude hiding (compare)

data Value = ValueUser User
           | ValueArtists [Artist]

newtype Limit = Limit Int deriving (Show, LastfmValue)

instance Show Value where
  show (ValueUser _)    = "user"
  show (ValueArtists _) = "artists"

instance LastfmValue Value where
  unpack (ValueUser u)     = unpack u
  unpack (ValueArtists as) = unpack as

compare :: Value -> Value -> Maybe Limit -> APIKey -> IO Response
compare value1 value2 limit apiKey = callAPI "tasteometer.compare"
  [ "type1" ?< (show value1)
  , "value1" ?< value1
  , "type2" ?< (show value2)
  , "value2" ?< value2
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

{- `compareGroup' method is deprecated -}
