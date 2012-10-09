{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Lastfm.JSON
  ( json
  , jsonWrapper
  ) where

import Control.Applicative ((<$>), empty)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Language.Haskell.TH

import Network.Lastfm.Internal (Format(..))
import Network.Lastfm.Error (LastfmError, disambiguate)


json ∷ Format
json = Format
  { errorParser = parser
  , uriArgument = Just ("format","json")
  }


parser ∷ ByteString → Maybe LastfmError
parser xs = case AP.parse A.json xs of
  AP.Done _ j → case A.parse p j of
    A.Success v → Just v
    _ → Nothing
  _ → Nothing
 where
  p (A.Object v) = disambiguate <$> v A..: "error"
  p _ = empty


jsonWrapper ∷ [String] → Q [Dec]
jsonWrapper = mapM func
  where
   func xs = funD (mkName xs) [clause [] (normalB $ appE (varE (mkName ("API." ++ xs))) [e| json |]) []]
