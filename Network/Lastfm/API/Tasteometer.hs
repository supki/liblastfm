-- | Tasteometer API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Tasteometer
  ( compare
  ) where

import Control.Monad.Error (runErrorT, throwError)
import Network.Lastfm
import Prelude hiding (compare)

(?<) :: Argument a => a -> Int -> (String, String)
a ?< n = (key a ++ show n, value a)

-- | Get a Tasteometer score from two inputs, along with a list of shared artists. If the input is a User some additional information is returned.
--
-- More: <http://www.lastfm.ru/api/show/tasteometer.compare>
compare :: Value -> Value -> Maybe Limit -> APIKey -> Lastfm Response
compare value1 value2 limit apiKey = runErrorT go
  where go
          | isNull value1 = throwError $ WrapperCallError method "empty first artists list."
          | isNull value2 = throwError $ WrapperCallError method "empty second artists list."
          | isExceededMaximum value1 = throwError $ WrapperCallError method "first artists list length has exceeded maximum (100)."
          | isExceededMaximum value2 = throwError $ WrapperCallError method "second artists list length has exceeded maximum (100)."
          | otherwise = callAPI
            [ (#) (Method method)
            , (,) "type1" (show value1)
            , (,) "type2" (show value2)
            , (?<) value1 1
            , (?<) value2 2
            , (#) limit
            , (#) apiKey
            ]
            where method = "tasteometer.compare"

                  isNull :: Value -> Bool
                  isNull (ValueArtists []) = True
                  isNull _ = False

                  isExceededMaximum :: Value -> Bool
                  isExceededMaximum (ValueUser _) = False
                  isExceededMaximum (ValueArtists as) = null . drop 100 $ as
