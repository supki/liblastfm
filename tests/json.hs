{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid
import System.Exit (ExitCode(ExitFailure), exitWith)

import qualified Data.ByteString.Lazy as B
import           Data.Aeson
import           Data.Text (Text)
import           Network.Lastfm
import           Test.Framework

import qualified Album as Album
import qualified Artist as Artist
import qualified Chart as Chart
import qualified Event as Event
import qualified Geo as Geo
import qualified Group as Group
import qualified Library as Library
import qualified Playlist as Playlist
import qualified Tag as Tag
import qualified Tasteometer as Tasteometer
import qualified Track as Track
import qualified User as User
import qualified Venue as Venue


main :: IO ()
main =
  do keys <- B.readFile "tests/lastfm-keys.json"
     case decode keys of
       Just (Keys ak sk s) ->
         defaultMainWithOpts (auth <> noauth) (mempty { ropt_threads = Just 20 })
          where
           auth = mconcat . map (\f -> f (apiKey ak) (sessionKey sk) (Secret s)) $
             [ Album.auth
             , Artist.auth
             , Event.auth
             , Library.auth
             , Playlist.auth
             , Track.auth
             , User.auth
             ]
           noauth = mconcat . map (\f -> f (apiKey ak)) $
             [ Album.noauth
             , Artist.noauth
             , Chart.noauth
             , Event.noauth
             , Geo.noauth
             , Group.noauth
             , Library.noauth
             , Tag.noauth
             , Tasteometer.noauth
             , Track.noauth
             , User.noauth
             , Venue.noauth
             ]
       Nothing -> exitWith (ExitFailure 1)


data Keys = Keys Text Text Text


instance FromJSON Keys where
  parseJSON (Object o) = Keys <$> (o .: "APIKey") <*> (o .: "SessionKey") <*> (o .: "Secret")
  parseJSON _ = empty
