{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Common where

import Data.Aeson.Types
import Data.Void (Void)
import Network.Lastfm
import Test.HUnit


check ∷ (Value → Parser a) → Request JSON Ready Void → Assertion
check p q = do
  r ← lastfm q
  case parse p `fmap` r of
    Just (Success _) → assertBool "success" True
    _                → assertFailure $ "Got: " ++ show r


ok ∷ Value → Parser String
ok o = parseJSON o >>= (.: "status")
