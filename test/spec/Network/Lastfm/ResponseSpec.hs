{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Lastfm.ResponseSpec (spec) where

import Control.Exception (ArithException(DivideByZero), throwIO)
import Control.Exception.Lens
import Control.Lens
import Control.Lens.Aeson
import Data.Proxy (Proxy(..))
import Data.Void (Void)
import Test.Hspec.Lens
import Text.Xml.Lens
import Network.HTTP.Conduit (HttpException(..))

import Network.Lastfm.Response


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

  describe "wrapHttpException" $ do
    it "catches 'HttpException'" $ do
      val <- wrapHttpException (throwIO ResponseTimeout) :: IO (Either LastfmError Void)
      val `shouldPreview` ResponseTimeout `through` _Left._LastfmHttpException

    it "does not catch other exceptions" $
      wrapHttpException (throwIO DivideByZero) `shouldThrow` _DivideByZero

  describe "parse" $ do
    context "JSON" $ do
      let proxy :: Proxy JSON
          proxy = Proxy

      it "handles good input" $
        let
          good = "{ \"a\": { \"b\": 4 } }"
        in
          parse proxy good `shouldPreview` 4 `through` _Right.key "a".key "b"._Integer

      it "handles malformed input" $
        let
          malformed = "not a json"
        in
          parse proxy malformed `shouldPreview` malformed `through` _Left._LastfmBadResponse

      it "handles input with encoded errors" $
        let
          encodedError = "{ \"error\": 5, \"message\": \"foo\" }"
        in
          parse proxy encodedError `shouldPreview` (5, "foo") `through` _Left._LastfmEncodedError

    context "XML" $ do
      let proxy :: Proxy XML
          proxy = Proxy

      it "handles good input" $
        let
          good = "<root><foo><bar>baz</bar></foo></root>"
        in
          parse proxy good `shouldPreview` "baz" `through` _Right.root.node "foo".node "bar".text

      it "handles malformed input" $
        let
          malformed = "not a xml"
        in
          parse proxy malformed `shouldPreview` malformed `through` _Left._LastfmBadResponse

      -- it "handles input with encoded errors" $
      --   let
      --     encodedError = "<lfm><error code=\"5\">foo</error></lfm>"
      --   in
      --     parse proxy encodedError `shouldPreview` (5, "foo") `through` _Left._LastfmEncodedError
