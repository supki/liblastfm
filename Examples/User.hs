#!/usr/bin/env runhaskell

import Control.Monad ((<=<))

import Network.Lastfm.Core
import Network.Lastfm.Types
import qualified Network.Lastfm.API.User as User

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user1 = User "smpcln"
user2 = User "mokele"
artist = Artist "Dvar"

pemis s = putStr $ "\n" ++ s

getArtistTracks :: IO ()
getArtistTracks = do response <- User.getArtistTracks user1 artist Nothing Nothing Nothing apiKey
                     pemis "Artist tracks: "
                     case response of
                       Left e -> print e
                       Right r -> print $ artistTracks r
  where artistTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "artisttracks"

getBannedTracks :: IO ()
getBannedTracks = do response <- User.getBannedTracks user1 Nothing (Just $ Limit 10) apiKey
                     pemis "Banned artists: "
                     case response of
                       Left e -> print e
                       Right r -> print $ bannedArtists r
  where bannedArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "bannedtracks"

getEvents :: IO ()
getEvents = do response <- User.getEvents user2 Nothing (Just $ Limit 5) Nothing apiKey
               pemis "Events: "
               case response of
                 Left e -> print e
                 Right r -> print $ events r
  where events = mapM (getContent <=< lookupChild "url" <=< lookupChild "venue") <=< lookupChildren "event" <=< lookupChild "events"

getFriends :: IO ()
getFriends = do response <- User.getFriends user1 Nothing Nothing (Just $ Limit 10) apiKey
                pemis "Friends: "
                case response of
                  Left e -> print e
                  Right r -> print $ friends r
  where friends = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "friends"

getPlayCount :: IO ()
getPlayCount = do response <- User.getInfo (Just user1) apiKey
                  pemis "Play count: "
                  case response of
                    Left e -> print e
                    Right r -> print $ playCount r
  where playCount = getContent <=< lookupChild "playcount" <=< lookupChild "user"

getLovedTracks :: IO ()
getLovedTracks = do response <- User.getLovedTracks user1 Nothing (Just $ Limit 10) apiKey
                    pemis "Loved tracks: "
                    case response of
                      Left e -> print e
                      Right r -> print $ lovedTracks r
  where lovedTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "lovedtracks"

getNeighbours :: IO ()
getNeighbours = do response <- User.getNeighbours user1 (Just $ Limit 10) apiKey
                   pemis "Neighbours: "
                   case response of
                     Left e -> print e
                     Right r -> print $ neighbours r
  where neighbours = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "neighbours"

getNewReleases :: IO ()
getNewReleases = do response <- User.getNewReleases user1 Nothing apiKey
                    pemis "New releases: "
                    case response of
                      Left e -> print e
                      Right r -> print $ newReleases r
  where newReleases = mapM (getContent <=< lookupChild "url") <=< lookupChildren "album" <=< lookupChild "albums"

getPastEvents :: IO ()
getPastEvents = do response <- User.getPastEvents user2 Nothing (Just $ Limit 5) apiKey
                   pemis "Past events: "
                   case response of
                     Left e -> print e
                     Right r -> print $ pastEvents r
  where pastEvents = mapM (getContent <=< lookupChild "url") <=< lookupChildren "event" <=< lookupChild "events"

main = do
  getArtistTracks
  getBannedTracks
  getEvents
  getFriends
  getPlayCount
  getLovedTracks
  getNeighbours
  getNewReleases
  getPastEvents
--  getPersonalTags
--  getPlaylists
--  getRecentStations
--  getRecentTracks
--  getRecommendedArtists
--  getRecommendedEvents
--  getShouts
--  getTopAlbums
--  getTopArtists
--  getTopFans
--  getTopTracks
--  getWeeklyAlbumChart
--  getWeeklyArtistChart
--  getWeeklyChartList
--  getWeeklyTrackChart
--  shout
