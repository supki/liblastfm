{-# LANGUAGE UnicodeSyntax #-}
module Main where

import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

import Test.HUnit
import qualified JSON.Album as Album
import qualified JSON.Artist as Artist
import qualified JSON.Chart as Chart
import qualified JSON.Event as Event
import qualified JSON.Geo as Geo


main ∷ IO ()
main =
  do rs ← mapM (runTestTT . TestList)
       [ Album.tests
       , Artist.tests
       , Chart.tests
       , Event.tests
       , Geo.tests
       ]
     let fs = sum $ map failures rs
     case fs of
       0 → exitSuccess
       n → exitWith (ExitFailure n)
