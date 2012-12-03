{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Applicative
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Data.Text.Lazy (Text)
import           Test.HUnit
import qualified Album as Album


main ∷ IO ()
main =
  do keys ← B.readFile "examples/lastfm-keys.json"
     case decode keys of
       Just (Keys ak sk s) →
         do rs ← mapM (runTestTT . TestList . \f → f ak sk s)
              [ Album.auth
              ]
            rs' ← mapM (runTestTT . TestList)
              [ Album.noauth
              ]
            let fs = sum (map failures rs) + sum (map failures rs')
            case fs of
              0 → exitSuccess
              n → exitWith (ExitFailure n)
       Nothing → exitWith (ExitFailure 127)


data Keys = Keys Text Text Text


instance FromJSON Keys where
  parseJSON (Object o) = Keys <$> (o .: "APIKey") <*> (o .: "SessionKey") <*> (o .: "Secret")
  parseJSON _ = empty
