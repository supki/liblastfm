{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Lastfm.JSON
  ( json
  , jsonWrapper
  ) where

import Control.Applicative ((<$>), empty)

import Data.Aeson hiding (json)
import Language.Haskell.TH

import Network.Lastfm.Internal (Format(..))
import Network.Lastfm.Error (LastfmError, disambiguate)


json ∷ Format
json = Format
        { errorParser = decode
        , uriArgument = ("format","json")
        }


instance FromJSON LastfmError where
  parseJSON (Object v) = disambiguate <$> v .: "error"
  parseJSON _ = empty


jsonWrapper ∷ [String] → Q [Dec]
jsonWrapper = mapM func
  where
   func xs = funD (mkName xs) [clause [] (normalB $ appE (varE (mkName ("API." ++ xs))) [e| json |]) []]
