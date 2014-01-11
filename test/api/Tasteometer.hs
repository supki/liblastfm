{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Tasteometer (noauth) where

import           Control.Lens.Aeson
import           Data.Text (Text)
import           Network.Lastfm
import qualified Network.Lastfm.Tasteometer as T
import           Test.Framework
import           Test.Framework.Providers.HUnit

import           Helper


noauth :: [Test]
noauth =
  [ testCase "Tasteometer.compare"    testCompare
  , testCase "Tasteometer.compare'"   testCompare'
  , testCase "Tasteometer.compare''"  testCompare''
  , testCase "Tasteometer.compare'''" testCompare'''
  ]
 where
  testCompare = query cs $
    T.compare (user "smpcln") (user "MCDOOMDESTROYER") <*> publicKey
  testCompare' = query cs $
    T.compare (user "smpcln") (artists ["enduser", "venetian snares"]) <*> publicKey
  testCompare'' = query cs $
    T.compare (artists ["enduser", "venetian snares"]) (user "smpcln") <*> publicKey
  testCompare''' = query cs $
    T.compare (artists ["enduser", "venetian snares"]) (artists ["enduser", "venetian snares"]) <*> publicKey


cs :: Query Text
cs = key "comparison".key "result".key "score"._String
