module Main where

import Control.Applicative (empty)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Network.Lastfm
import Test.HUnit
import qualified JSON.Album as Album
import qualified JSON.Artist as Artist
import qualified JSON.Event as Event
import qualified JSON.Library as Library


main ∷ IO ()
main =
  do keys ← B.readFile "examples/lastfm-keys.json"
     case decode keys of
       Just (Keys ak sk s) →
         do rs ← mapM (runTestTT . TestList . \f → f ak sk s)
              [ Album.private
              , Artist.private
              , Event.private
              , Library.private
              ]
            let fs = sum $ map failures rs
            case fs of
              0 → exitSuccess
              n → exitWith (ExitFailure n)
       Nothing → exitWith (ExitFailure 127)


data Keys = Keys APIKey SessionKey Secret


instance FromJSON Keys where
  parseJSON (Object o) =
    do ak ← o .: "APIKey"
       sk ← o .: "SessionKey"
       s ← o .: "Secret"
       return $ Keys (APIKey ak) (SessionKey sk) (Secret s)
  parseJSON _ = empty
