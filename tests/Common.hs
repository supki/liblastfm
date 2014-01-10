{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Aeson.Types
import Network.Lastfm (Request, Format(..), Ready, lastfm)
import Test.HUnit


check :: (Value -> Parser a) -> Request JSON Ready -> Assertion
check p q = do
  r <- lastfm q
  case parse p `fmap` r of
    Right (Success _) -> assertBool "success" True
    _                 -> assertFailure $ "Got: " ++ show r


ok :: Value -> Parser String
ok o = parseJSON o >>= (.: "status")
