{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Network.Lastfm.Types.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

newtypes ∷ String → [String] → Q [Dec]
newtypes (mkName → t) (map mkName → ns) = mapM (newtypeDeclaration t) ns
  where newtypeDeclaration t n = newtypeD (cxt []) n [] (normalC n [strictType notStrict (conT t)]) []
