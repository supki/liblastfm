{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.TasteometerSpec (spec) where

import           Data.Text (Text)
import           Network.Lastfm
import qualified Network.Lastfm.Tasteometer as Taste
import           Test.Hspec
import           Text.Xml.Lens

import           SpecHelper


spec :: Spec
spec = do
  it "compare" $
    publicly (Taste.compare (user "smpcln") (user "MCDOOMDESTROYER"))
   `shouldHaveXml`
    xmlQuery

  it "compare" $
    publicly (Taste.compare (user "smpcln") (artists ["enduser", "venetian snares"]))
   `shouldHaveXml`
    xmlQuery

  it "compare" $
    publicly (Taste.compare (artists ["enduser", "venetian snares"]) (user "smpcln"))
   `shouldHaveXml`
    xmlQuery

  it "compare" $
    publicly (Taste.compare (artists ["enduser", "venetian snares"]) (artists ["enduser", "venetian snares"]))
   `shouldHaveXml`
    xmlQuery

xmlQuery :: Fold Document Text
xmlQuery = root.node "comparison".node "result".node "score".text
