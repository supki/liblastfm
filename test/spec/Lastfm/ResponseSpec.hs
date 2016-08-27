{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lastfm.ResponseSpec (spec) where

import Control.Exception (ArithException(..), throwIO, try)
import Control.Exception.Lens
import Control.Lens
import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson.Lens
import Test.Hspec.Lens
import Text.Xml.Lens
import Network.HTTP.Client (HttpException(..))

import Lastfm.Response


instance Eq HttpException where
  _ == _ = True


spec :: Spec
spec = do
  describe "md5" $ do
    it "calculates the right hash for an empty string" $
      md5 "" `shouldBe` "d41d8cd98f00b204e9800998ecf8427e"

    it "calculates the right hash for an ascii string" $
      md5 "Enduser" `shouldBe` "28ced1fafec20ae302a04e9f27f4800f"

    it "calculates the right hash for a unicode string" $
      md5 "ДМИТРИЙ МАЛИКОВ" `shouldBe` "e02e5affef1004a02d9762619dc2a585"

  describe "try for LastfmError" $ do
    let tryLastfmError :: IO a -> IO (Either LastfmError a)
        tryLastfmError = try

    it "catches 'HttpException'" $ do
      let ex = InvalidUrlException "foo" "bar"
      val <- tryLastfmError (throwIO ex) :: IO (Either LastfmError ())
      val `shouldHave` _Left._LastfmHttpError.only ex

    it "does not catch other exceptions" $
      tryLastfmError (throwIO DivideByZero) `shouldThrow` _DivideByZero

  describe "parse" $ do
    context "JSON" $ do
      let parseJSON :: ByteString -> Either LastfmError Value
          parseJSON = parse

      it "handles good input" $
        let
          good = "{ \"a\": { \"b\": 4 } }"
        in
          parseJSON good `shouldHave` _Right.key "a".key "b"._Integer.only 4

      it "handles malformed input" $
        let
          malformed = "not a json"
        in
          parseJSON malformed `shouldHave` _Left._LastfmBadResponse.only malformed

      it "handles input with encoded errors" $
        let
          encodedError = "{ \"error\": 5, \"message\": \"foo\" }"
        in
          parseJSON encodedError `shouldHave` _Left._LastfmEncodedError.only (5, "foo")

    context "XML" $ do
      let parseXML :: ByteString -> Either LastfmError Document
          parseXML = parse

      it "handles good input" $
        let
          good = "<root><foo><bar>baz</bar></foo></root>"
        in
          parseXML good `shouldHave` _Right.root.node "foo".node "bar".text.only "baz"

      it "handles malformed input" $
        let
          malformed = "not a xml"
        in
          parseXML malformed `shouldHave` _Left._LastfmBadResponse.only malformed

      it "handles input with encoded errors" $
        let
          encodedError = "<lfm><error code=\"5\">foo</error></lfm>"
        in
          parseXML encodedError `shouldHave` _Left._LastfmEncodedError.only (5, "foo")
