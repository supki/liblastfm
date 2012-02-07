module EUser (start) where

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.User as User

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user1 = User "smpcln"
user2 = User "mokele"
user3 = User "rj"
artist = Artist "Dvar"

pemis s = putStr $ "\n" ++ s

getArtistTracks :: IO ()
getArtistTracks = do response <- User.getArtistTracks user1 artist Nothing Nothing Nothing apiKey
                     pemis "Artist tracks: "
                     case response of
                       Left e -> print e
                       Right r -> print $ artistTracks r
  where artistTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "artisttracks" <=< wrap

getBannedTracks :: IO ()
getBannedTracks = do response <- User.getBannedTracks user1 Nothing (Just $ Limit 10) apiKey
                     pemis "Banned artists: "
                     case response of
                       Left e -> print e
                       Right r -> print $ bannedArtists r
  where bannedArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "bannedtracks" <=< wrap

getEvents :: IO ()
getEvents = do response <- User.getEvents user2 Nothing (Just $ Limit 5) Nothing apiKey
               pemis "Events: "
               case response of
                 Left e -> print e
                 Right r -> print $ events r
  where events = mapM (getContent <=< lookupChild "url" <=< lookupChild "venue") <=< lookupChildren "event" <=< lookupChild "events" <=< wrap

getFriends :: IO ()
getFriends = do response <- User.getFriends user1 Nothing Nothing (Just $ Limit 10) apiKey
                pemis "Friends: "
                case response of
                  Left e -> print e
                  Right r -> print $ friends r
  where friends = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "friends" <=< wrap

getPlayCount :: IO ()
getPlayCount = do response <- User.getInfo (Just user1) apiKey
                  pemis "Play count: "
                  case response of
                    Left e -> print e
                    Right r -> print $ playCount r
  where playCount = getContent <=< lookupChild "playcount" <=< lookupChild "user" <=< wrap

getLovedTracks :: IO ()
getLovedTracks = do response <- User.getLovedTracks user1 Nothing (Just $ Limit 10) apiKey
                    pemis "Loved tracks: "
                    case response of
                      Left e -> print e
                      Right r -> print $ lovedTracks r
  where lovedTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "lovedtracks" <=< wrap

getNeighbours :: IO ()
getNeighbours = do response <- User.getNeighbours user1 (Just $ Limit 10) apiKey
                   pemis "Neighbours: "
                   case response of
                     Left e -> print e
                     Right r -> print $ neighbours r
  where neighbours = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "neighbours" <=< wrap

getNewReleases :: IO ()
getNewReleases = do response <- User.getNewReleases user1 Nothing apiKey
                    pemis "New releases: "
                    case response of
                      Left e -> print e
                      Right r -> print $ newReleases r
  where newReleases = mapM (getContent <=< lookupChild "url") <=< lookupChildren "album" <=< lookupChild "albums" <=< wrap

getPastEvents :: IO ()
getPastEvents = do response <- User.getPastEvents user2 Nothing (Just $ Limit 5) apiKey
                   pemis "Past events: "
                   case response of
                     Left e -> print e
                     Right r -> print $ pastEvents r
  where pastEvents = mapM (getContent <=< lookupChild "url") <=< lookupChildren "event" <=< lookupChild "events" <=< wrap

getPersonalTags :: IO ()
getPersonalTags = do response <- User.getPersonalTags user2 (Tag "rock") (TaggingType "artist") Nothing (Just $ Limit 10) apiKey
                     pemis "Personal tags: "
                     case response of
                       Left e -> print e
                       Right r -> print $ personalTags r
  where personalTags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists" <=< lookupChild "taggings" <=< wrap

getPlaylists :: IO ()
getPlaylists = do response <- User.getPlaylists user2 apiKey
                  pemis "Playlists: "
                  case response of
                    Left e -> print e
                    Right r -> print $ playlists r
  where playlists = mapM (getContent <=< lookupChild "title") <=< lookupChildren "playlist" <=< lookupChild "playlists" <=< wrap

{-- requires autorization
getRecentStations :: IO ()
 --}

getRecentTracks :: IO ()
getRecentTracks = do response <- User.getRecentTracks user1 Nothing (Just $ Limit 10) Nothing Nothing apiKey
                     pemis "Recent tracks: "
                     case response of
                       Left e -> print e
                       Right r -> print $ recentTracks r
  where recentTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "recenttracks" <=< wrap

{-- requires autorization
getRecommendedArtists
 --}

{-- requires autorization
getRecommendedEvents
 --}

getShouts :: IO ()
getShouts = do response <- User.getShouts user1 Nothing (Just $ Limit 1) apiKey
               pemis "Shouts: "
               case response of
                 Left e -> print e
                 Right r -> print $ shouts r
  where shouts = mapM (getContent <=< lookupChild "body") <=< lookupChildren "shout" <=< lookupChild "shouts" <=< wrap

getTopAlbums :: IO ()
getTopAlbums = do response <- User.getTopAlbums user1 Nothing Nothing (Just $ Limit 5) apiKey
                  pemis "Top albums: "
                  case response of
                    Left e -> print e
                    Right r -> print $ topAlbums r
  where topAlbums = mapM (getContent <=< lookupChild "name" <=< lookupChild "artist") <=< lookupChildren "album" <=< lookupChild "topalbums" <=< wrap

getTopArtists :: IO ()
getTopArtists = do response <- User.getTopArtists user1 Nothing Nothing (Just $ Limit 5) apiKey
                   pemis "Top artists: "
                   case response of
                     Left e -> print e
                     Right r -> print $ topArtists r
  where topArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists" <=< wrap

getTopTags :: IO ()
getTopTags = do response <- User.getTopTags user1 (Just $ Limit 10) apiKey
                pemis "Top tags: "
                case response of
                  Left e -> print e
                  Right r -> print $ topTags r
  where topTags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "toptags" <=< wrap

getTopTracks :: IO ()
getTopTracks = do response <- User.getTopTracks user1 Nothing Nothing (Just $ Limit 10) apiKey
                  pemis "Top tracks: "
                  case response of
                    Left e -> print e
                    Right r -> print $ topTracks r
  where topTracks = mapM (getContent <=< lookupChild "url") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

getWeeklyAlbumChart :: IO ()
getWeeklyAlbumChart = do response <- User.getWeeklyAlbumChart user3 Nothing Nothing apiKey
                         pemis "Weekly album chart: "
                         case response of
                           Left e -> print e
                           Right r -> print $ weeklyAlbumChart r
  where weeklyAlbumChart = mapM (getContent <=< lookupChild "url") <=< lookupChildren "album" <=< lookupChild "weeklyalbumchart" <=< wrap

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = do response <- User.getWeeklyArtistChart user3 Nothing Nothing apiKey
                          pemis "Weekly artist chart: "
                          case response of
                            Left e -> print e
                            Right r -> print $ weeklyArtistChart r
  where weeklyArtistChart = mapM (getContent <=< lookupChild "url") <=< lookupChildren "artist" <=< lookupChild "weeklyartistchart" <=< wrap

{-
getWeeklyChartList :: IO ()
getWeeklyChartList = do response <- User.getWeeklyChartList user3 apiKey
                        pemis "Weekly chart list: "
                        case response of
                          Left e -> print e
                          Right r -> print $ take 10 r
-}

getWeeklyTrackChart :: IO ()
getWeeklyTrackChart = do response <- User.getWeeklyTrackChart user3 Nothing Nothing apiKey
                         pemis "Weekly track chart: "
                         case response of
                           Left e -> print e
                           Right r -> print $ weeklyTrackChart r
  where weeklyTrackChart = mapM (getContent <=< lookupChild "url") <=< lookupChildren "track" <=< lookupChild "weeklytrackchart" <=< wrap

{-- requires autorization
shout
 --}

start :: IO ()
start = do getArtistTracks
           getBannedTracks
           getEvents
           getFriends
           getPlayCount
           getLovedTracks
           getNeighbours
           getNewReleases
           getPastEvents
           getPersonalTags
           getPlaylists
           -- getRecentStations
           getRecentTracks
           -- getRecommendedArtists
           -- getRecommendedEvents
           getShouts
           getTopAlbums
           getTopArtists
           getTopTags
           getTopTracks
           getWeeklyAlbumChart
           getWeeklyArtistChart
           -- getWeeklyChartList
           getWeeklyTrackChart
           -- shout
