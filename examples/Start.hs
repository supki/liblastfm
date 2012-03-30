#!/usr/bin/env runhaskell

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.Console.GetOpt (getOpt, usageInfo, ArgOrder (Permute), OptDescr (Option), ArgDescr (NoArg))
import System.IO (hPutStrLn, stderr)

import qualified EAlbum as Album
import qualified EArtist as Artist
import qualified EChart as Chart
import qualified EEvent as Event
import qualified EGeo as Geo
import qualified EGroup as Group
import qualified ELibrary as Library
import qualified EPlaylist as Playlist
import qualified ERadio as Radio
import qualified ERadio as Radio
import qualified ETag as Tag
import qualified ETasteometer as Tasteometer
import qualified ETrack as Track
import qualified EUser as User
import qualified EVenue as Venue

import Network.Lastfm.Types (APIKey(..), SessionKey(..), Secret(..))

data Flag
       = Help
       | All
       | Album
       | Artist
       | Chart
       | Event
       | Geo
       | Group
       | Library
       | Playlist
       | Radio
       | Tag
       | Tasteometer
       | Track
       | User
       | Venue
         deriving Eq

options :: [OptDescr Flag]
options =
  [ Option "h" ["help"]       (NoArg Help)        "Print this help message and exit."
  , Option "" ["all"]         (NoArg All)         "Start all modules examples."
  , Option "" ["album"]       (NoArg Album)       "Start Album module examples."
  , Option "" ["artist"]      (NoArg Artist)      "Start Artist module examples."
  , Option "" ["chart"]       (NoArg Chart)       "Start Chart module examples."
  , Option "" ["event"]       (NoArg Event)       "Start Event module examples."
  , Option "" ["geo"]         (NoArg Geo)         "Start Geo module examples."
  , Option "" ["group"]       (NoArg Group)       "Start Group module examples."
  , Option "" ["library"]     (NoArg Library)     "Start Library module examples."
  , Option "" ["playlist"]    (NoArg Playlist)    "Start Playlist module examples."
  , Option "" ["radio"]       (NoArg Radio)       "Start Radio module examples."
  , Option "" ["tag"]         (NoArg Tag)         "Start Tag module examples."
  , Option "" ["tasteometer"] (NoArg Tasteometer) "Start Tasteometer module examples."
  , Option "" ["track"]       (NoArg Track)       "Start Track module examples."
  , Option "" ["user"]        (NoArg User)        "Start User module examples."
  , Option "" ["venue"]       (NoArg Venue)       "Start Venue module examples."
  ]


parseArgs :: [String] -> IO [Flag]
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) -> if All `elem` os then return all else filterM check os
  (_, _, es)  -> hPutStrLn stderr (concat es ++ usageInfo header options) >> exitWith (ExitFailure 1)
  where
    all :: [Flag]
    all = [Album, Artist, Chart, Event, Geo, Group, Library, Playlist, Radio, Tag, Tasteometer, Track, User, Venue]

    check :: Flag -> IO Bool
    check Help    = hPutStrLn stderr (usageInfo header options) >> exitWith ExitSuccess
    check _       = return True

    header = "Usage: ./start [MODULE|--all]"

getConfig :: FilePath -> IO (APIKey, SessionKey, Secret)
getConfig fp = do [apiKey, sessionKey, secret] <- map ((!! 1) . splitOn "=" . filter (not . isSpace)) . lines <$> readFile fp
                  return (APIKey apiKey, SessionKey sessionKey, Secret secret)

common :: Flag -> IO ()
common Album = Album.common
common Artist = Artist.common
common Chart = Chart.common
common Event = Event.common
common Geo = Geo.common
common Group = Group.common
common Library = Library.common
common Playlist = Playlist.common
common Radio = Radio.common
common Tag = Tag.common
common Tasteometer = Tasteometer.common
common Track = Track.common
common User = User.common
common Venue = Venue.common

auth :: Flag -> APIKey -> SessionKey -> Secret -> IO ()
auth Album = Album.auth
auth Artist = Artist.auth
auth Chart = Chart.auth
auth Event = Event.auth
auth Geo = Geo.auth
auth Group = Group.auth
auth Library = Library.auth
auth Playlist = Playlist.auth
auth Radio = Radio.auth
auth Tag = Tag.auth
auth Tasteometer = Tasteometer.auth
auth Track = Track.auth
auth User = User.auth
auth Venue = Venue.auth

main :: IO ()
main = do args <- getArgs
          modules <- parseArgs args
          (a, sk, s) <- getConfig ".lastfm.conf"
          mapM_ (\m -> common m >> auth m a sk s) modules
