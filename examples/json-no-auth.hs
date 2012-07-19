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
import qualified JSON.Radio as Radio
import qualified JSON.Tag as Tag
import qualified JSON.Tasteometer as Tasteometer
import qualified JSON.Venue as Venue
import qualified JSON.User as User


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
       , Radio.public
       , Tag.public
       , Tasteometer.public
       , Venue.public
       , User.public
       ]
     let fs = sum $ map failures rs
     case fs of
       0 → exitSuccess
       n → exitWith (ExitFailure n)
