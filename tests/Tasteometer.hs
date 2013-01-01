{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Tasteometer (noauth) where

import           Data.Aeson.Types
import           Network.Lastfm
import qualified Network.Lastfm.Tasteometer as T
import           Test.Framework
import           Test.Framework.Providers.HUnit

import Common


noauth ∷ Request JSON Send APIKey → [Test]
noauth ak =
  [ testCase "Tasteometer.compare"    testCompare
  , testCase "Tasteometer.compare'"   testCompare'
  , testCase "Tasteometer.compare''"  testCompare''
  , testCase "Tasteometer.compare'''" testCompare'''
  ]
 where
  testCompare = check cs $
    T.compare (user "smpcln") (user "MCDOOMDESTROYER") <*> ak
  testCompare' = check cs $
    T.compare (user "smpcln") (artists ["enduser", "venetian snares"]) <*> ak
  testCompare'' = check cs $
    T.compare (artists ["enduser", "venetian snares"]) (user "smpcln") <*> ak
  testCompare''' = check cs $
    T.compare (artists ["enduser", "venetian snares"]) (artists ["enduser", "venetian snares"]) <*> ak


cs ∷ Value → Parser String
cs o = parseJSON o >>= (.: "comparison") >>= (.: "result") >>= (.: "score")
