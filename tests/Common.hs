{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common where

import Data.Aeson.Types
import Test.HUnit


instance Assertable (Maybe (Result a)) where
  assert (Just (Success _)) = assertBool "always success" True
  assert _ = assertFailure "cannot parse JSON"


(<:>) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(<:>) = fmap . fmap

ok ∷ Value → Parser String
ok o = parseJSON o >>= (.: "status")
