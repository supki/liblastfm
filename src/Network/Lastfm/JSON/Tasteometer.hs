{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Tasteometer API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Tasteometer
  ( compare
  ) where

#include "tasteometer.docs"

import Prelude hiding (compare)

import Network.Lastfm.Internal
import Network.Lastfm.JSON (jsonWrapper)
import qualified Network.Lastfm.API.Tasteometer as API

$(jsonWrapper ["compare"])

__compare__
compare ∷ Value → Value → Maybe Limit → APIKey → Lastfm Response
