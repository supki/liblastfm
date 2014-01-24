{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module TasteometerSpec (spec) where

import           Control.Lens.Aeson
import           Data.Text (Text)
import           Network.Lastfm
import qualified Network.Lastfm.Tasteometer as T
import           Test.Hspec

import           SpecHelper


spec :: Spec
spec = do
  it "Tasteometer.compare" $
    query cs $
      T.compare (user "smpcln") (user "MCDOOMDESTROYER") <*> publicKey

  it "Tasteometer.compare" $
    query cs $
      T.compare (user "smpcln") (artists ["enduser", "venetian snares"]) <*> publicKey

  it "Tasteometer.compare" $
    query cs $
      T.compare (artists ["enduser", "venetian snares"]) (user "smpcln") <*> publicKey

  it "Tasteometer.compare" $
    query cs $
      T.compare (artists ["enduser", "venetian snares"]) (artists ["enduser", "venetian snares"]) <*> publicKey

cs :: Query Text
cs = key "comparison".key "result".key "score"._String
