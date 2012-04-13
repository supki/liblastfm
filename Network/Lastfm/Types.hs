{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances #-}
module Network.Lastfm.Types
  ( module Network.Lastfm.Types.Argument
  , module Network.Lastfm.Types.Error
  , module Network.Lastfm.Types.Types
  , Secret(..)
  ) where

import Network.Lastfm.Types.Argument
import Network.Lastfm.Types.Error
import Network.Lastfm.Types.Types

newtype Secret = Secret String
