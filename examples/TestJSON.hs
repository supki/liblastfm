{-# LANGUAGE UnicodeSyntax #-}
module TestJSON where

import Test.HUnit
import qualified JSON.Album as Album
import qualified JSON.Artist as Artist
import qualified JSON.Chart as Chart
import qualified JSON.Event as Event
import qualified JSON.Geo as Geo


main ∷ IO ()
main =
  do runTestTT (TestList Album.tests)
     runTestTT (TestList Artist.tests)
     runTestTT (TestList Chart.tests)
     runTestTT (TestList Event.tests)
     runTestTT (TestList Geo.tests)
     return ()
