{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

import Test.HUnit
import qualified JSON.Album as Album
import qualified JSON.Artist as Artist
import qualified JSON.Chart as Chart
import qualified JSON.Event as Event
import qualified JSON.Geo as Geo
import qualified JSON.Group as Group
import qualified JSON.Library as Library


main ∷ IO ()
main =
  do rs ← mapM (runTestTT . TestList)
       [ Album.public
       , Artist.public
       , Chart.public
       , Event.public
       , Geo.public
       , Group.public
       , Library.public
       ]
     let fs = sum $ map failures rs
     case fs of
       0 → exitSuccess
       n → exitWith (ExitFailure n)
