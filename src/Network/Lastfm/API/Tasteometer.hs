module Network.Lastfm.API.Tasteometer
  ( compare
  ) where

import Network.Lastfm
import Prelude hiding (compare)

(?<) ∷ Argument a ⇒ a → Int → (String, String)
a ?< n = (key a ++ show n, value a)

compare ∷ ResponseType → Value → Value → Maybe Limit → APIKey → Lastfm Response
compare t value1 value2 limit apiKey = callAPI t
  [ (#) (Method "tasteometer.compare")
  , (,) "type1" (show value1)
  , (,) "type2" (show value2)
  , (?<) value1 1
  , (?<) value2 2
  , (#) limit
  , (#) apiKey
  ]
