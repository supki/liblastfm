{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import qualified Data.ByteString.Lazy as ByteString
import           Data.Aeson
import           Data.Monoid
import           Data.Text (Text)
import           Network.Lastfm
import           System.Exit (ExitCode(ExitFailure), exitWith)
import           Test.Framework

import qualified Album
import qualified Artist
import qualified Chart
import qualified Event
import qualified Geo
import qualified Group
import qualified Library
import qualified Playlist
import qualified Tag
import qualified Tasteometer
import qualified Track
import qualified User
import qualified Venue


main :: IO ()
main =
  do keys <- ByteString.readFile "test/api/lastfm-keys.json"
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
