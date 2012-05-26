{-# LANGUAGE TemplateHaskell #-}
-- | Tasteometer API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Tasteometer
  ( compare
  ) where

import Prelude hiding (compare)

import Network.Lastfm
import qualified Network.Lastfm.API.Tasteometer as API

$(json ["compare"])

-- | Get a Tasteometer score from two inputs, along with a list of shared artists. If the input is a User some additional information is returned.
--
-- More: <http://www.last.fm/api/show/tasteometer.compare>
compare ∷ Value → Value → Maybe Limit → APIKey → Lastfm Response
