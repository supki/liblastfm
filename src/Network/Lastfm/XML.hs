{-# LANGUAGE TemplateHaskell #-}
module Network.Lastfm.XML
  ( xml
  , xmlWrapper
  ) where

import Control.Applicative ((<$>), empty)
import Control.Monad ((<=<))

import Data.ByteString.Lazy.Char8 (ByteString)
import Language.Haskell.TH
import Text.XML.Light

import Network.Lastfm.Error (LastfmError, disambiguate)
import Network.Lastfm.Internal (Format(..))


xml ∷ Format
xml = Format
        { errorParser = xmlErrorParser
        , uriArgument = ("format","xml")
        }


xmlWrapper ∷ [String] → Q [Dec]
xmlWrapper = mapM func
  where
   func xs = funD (mkName xs) [clause [] (normalB $ appE (varE (mkName ("API." ++ xs))) [e| xml |]) []]


xmlErrorParser ∷ ByteString → Maybe LastfmError
xmlErrorParser r = disambiguate . read <$> (findAttr (unqual "code") <=< findChild (unqual "error") <=< parseXMLDoc) r
