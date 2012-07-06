{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Network.Lastfm.TH
  ( ResponseType(..)
  , xml, json
  , instances, newtypes
  ) where

import Language.Haskell.TH


-- Desired type of Lastfm response
data ResponseType = XML | JSON


-- Construct XML wrapper to specified API function
xml ∷ [String] → Q [Dec]
xml = mapM func
  where
   func xs = funD (mkName xs) [clause [] (normalB $ appE (varE (mkName ("API." ++ xs))) [e| XML |]) []]


-- Construct JSON wrapper to specified API function
json ∷ [String] → Q [Dec]
json = mapM func
  where
   func xs = funD (mkName xs) [clause [] (normalB $ appE (varE (mkName ("API." ++ xs))) [e| JSON |]) []]


instances ∷ String → [(String, String)] → Q [Dec]
instances f = mapM (instanceDeclaration "Argument")
  where
   instanceDeclaration (mkName → tc) (mkName → n, m) = instanceD (cxt []) (appT (conT tc) (conT n)) [first, second]
    where
     first = funD (mkName "key") [clause [] (normalB [e| const m |]) []]
     second = let var = mkName "a"
                  func = mkName f
              in funD (mkName "value") [clause [conP n [varP var]] (normalB $ appE (varE func) (varE var)) []]


newtypes ∷ String → [String] → Q [Dec]
newtypes (mkName → t) (map mkName → ns) = mapM newtypeDeclaration ns
  where
   newtypeDeclaration n = newtypeD (cxt []) n [] (normalC n [strictType notStrict (conT t)]) []
