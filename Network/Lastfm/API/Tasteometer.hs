module Network.Lastfm.API.Tasteometer
  ( compare
  ) where

import Control.Exception (throw)
import Prelude hiding (compare)

import Network.Lastfm.Response
import Network.Lastfm.Types ((?<), APIKey, Limit, Value(..))

compare :: Value -> Value -> Maybe Limit -> APIKey -> Lastfm Response
compare value1 value2 limit apiKey = dispatch go
  where go
          | isNull value1 = throw $ WrapperCallError method "empty first artists list."
          | isNull value2 = throw $ WrapperCallError method "empty second artists list."
          | isExceededMaximum value1 = throw $ WrapperCallError method "first artists list length has exceeded maximum (100)."
          | isExceededMaximum value2 = throw $ WrapperCallError method "second artists list length has exceeded maximum (100)."
          | otherwise = callAPI method
            [ "type1" ?< type1
            , "type2" ?< type2
            , "value1" ?< value1
            , "value2" ?< value2
            , "limit" ?< limit
            , "api_key" ?< apiKey
            ]
            where method = "tasteometer.compare"

                  type1 = show value1
                  type2 = show value2

                  isNull :: Value -> Bool
                  isNull (ValueUser _) = False
                  isNull (ValueArtists as) = null as

                  isExceededMaximum :: Value -> Bool
                  isExceededMaximum (ValueUser _) = False
                  isExceededMaximum (ValueArtists as) = length as > 100

{- `compareGroup' method is deprecated -}
