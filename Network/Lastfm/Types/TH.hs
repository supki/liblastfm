{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Network.Lastfm.Types.TH where

import Control.Applicative ((<$>))
import Language.Haskell.TH

instances ∷ String → [(String, String)] → Q [Dec]
instances f = mapM (instanceDeclaration "Argument")
  where instanceDeclaration (mkName → tc) (mkName → n, m) = instanceD (cxt []) (appT (conT tc) (conT n)) [first, second]
          where first = funD (mkName "key") [clause [] (normalB [e| const m |]) []]
                second = let var = mkName "a"
                             func = mkName f
                         in funD (mkName "value") [clause [conP n [varP var]] (normalB $ appE (varE func) (varE var)) []] 

newtypes ∷ String → [String] → Q [Dec]
newtypes (mkName → t) (map mkName → ns) = mapM (newtypeDeclaration t) ns
  where newtypeDeclaration t n = newtypeD (cxt []) n [] (normalC n [strictType notStrict (conT t)]) []
