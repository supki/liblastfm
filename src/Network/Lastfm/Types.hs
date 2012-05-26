module Network.Lastfm.Types
  ( module T
  , Secret(..)
  ) where

import Network.Lastfm.Types.Argument as T
import Network.Lastfm.Types.Error as T
import Network.Lastfm.Types.Types as T

newtype Secret = Secret String
