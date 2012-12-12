{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Control.Applicative
import Data.Monoid
import System.Exit (ExitCode(ExitFailure), exitWith)

import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Data.Text.Lazy (Text)
import           Test.Framework
import qualified Album as Album
import qualified Artist as Artist
import qualified Chart as Chart
import qualified Event as Event
import qualified Geo as Geo
import qualified Tag as Tag
import qualified Track as Track


main ∷ IO ()
main =
  do keys ← B.readFile "tests/lastfm-keys.json"
     case decode keys of
       Just (Keys ak sk s) →
         defaultMainWithOpts (auth <> noauth) (mempty { ropt_threads = Just 20 })
          where
           auth = mconcat . map (\f -> f ak sk s) $
             [ Album.auth
             , Artist.auth
             , Event.auth
             , Track.auth
             ]
           noauth = mconcat
             [ Album.noauth
             , Artist.noauth
             , Chart.noauth
             , Event.noauth
             , Geo.noauth
             , Tag.noauth
             , Track.noauth
             ]
       Nothing → exitWith (ExitFailure 1)


data Keys = Keys Text Text Text


instance FromJSON Keys where
  parseJSON (Object o) = Keys <$> (o .: "APIKey") <*> (o .: "SessionKey") <*> (o .: "Secret")
  parseJSON _ = empty
