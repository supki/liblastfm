-- | Tasteometer API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Tasteometer
  ( compare
  ) where

import Control.Monad.Error (runErrorT)
import Network.Lastfm
import Prelude hiding (compare)

(?<) ∷ Argument a ⇒ a → Int → (String, String)
a ?< n = (key a ++ show n, value a)

-- | Get a Tasteometer score from two inputs, along with a list of shared artists. If the input is a User some additional information is returned.
--
-- More: <http://www.last.fm/api/show/tasteometer.compare>
compare ∷ Value → Value → Maybe Limit → APIKey → Lastfm Response
compare value1 value2 limit apiKey = callAPI XML
  [ (#) (Method "tasteometer.compare")
  , (,) "type1" (show value1)
  , (,) "type2" (show value2)
  , (?<) value1 1
  , (?<) value2 2
  , (#) limit
  , (#) apiKey
  ]
