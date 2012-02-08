module EUser (start) where

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.User as User

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getArtistTracks :: IO ()
getArtistTracks = do response <- User.getArtistTracks (User "smpcln") (Artist "Dvar") Nothing Nothing Nothing apiKey
                     putStr "Artist tracks: "
                     case response of
                       Left e  -> print e
                       Right r -> print $ artistTracks r
                     putStrLn ""
  where artistTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "artisttracks" <=< wrap

getBannedTracks :: IO ()
getBannedTracks = do response <- User.getBannedTracks (User "smpcln") Nothing (Just $ Limit 10) apiKey
                     putStr "Banned artists: "
                     case response of
                       Left e  -> print e
                       Right r -> print $ bannedArtists r
                     putStrLn ""
  where bannedArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "bannedtracks" <=< wrap

getEvents :: IO ()
getEvents = do response <- User.getEvents (User "mokele") Nothing (Just $ Limit 5) Nothing apiKey
               putStr "Events: "
               case response of
                 Left e  -> print e
                 Right r -> print $ events r
               putStrLn ""
  where events = mapM (getContent <=< lookupChild "url" <=< lookupChild "venue") <=< lookupChildren "event" <=< lookupChild "events" <=< wrap

getFriends :: IO ()
getFriends = do response <- User.getFriends (User "smpcln") Nothing Nothing (Just $ Limit 10) apiKey
                putStr "Friends: "
                case response of
                  Left e  -> print e
                  Right r -> print $ friends r
                putStrLn ""
  where friends = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "friends" <=< wrap

getPlayCount :: IO ()
getPlayCount = do response <- User.getInfo (Just (User "smpcln")) apiKey
                  putStr "Play count: "
                  case response of
                    Left e  -> print e
                    Right r -> print $ playCount r
                  putStrLn ""
  where playCount = getContent <=< lookupChild "playcount" <=< lookupChild "user" <=< wrap

getLovedTracks :: IO ()
getLovedTracks = do response <- User.getLovedTracks (User "smpcln") Nothing (Just $ Limit 10) apiKey
                    putStr "Loved tracks: "
                    case response of
                      Left e  -> print e
                      Right r -> print $ lovedTracks r
                    putStrLn ""
  where lovedTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "lovedtracks" <=< wrap

getNeighbours :: IO ()
getNeighbours = do response <- User.getNeighbours (User "smpcln") (Just $ Limit 10) apiKey
                   putStr "Neighbours: "
                   case response of
                     Left e  -> print e
                     Right r -> print $ neighbours r
                   putStrLn ""
  where neighbours = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "neighbours" <=< wrap

getNewReleases :: IO ()
getNewReleases = do response <- User.getNewReleases (User "smpcln") Nothing apiKey
                    putStr "New releases: "
                    case response of
                      Left e  -> print e
                      Right r -> print $ newReleases r
                    putStrLn ""
  where newReleases = mapM (getContent <=< lookupChild "url") <=< lookupChildren "album" <=< lookupChild "albums" <=< wrap

getPastEvents :: IO ()
getPastEvents = do response <- User.getPastEvents (User "mokele") Nothing (Just $ Limit 5) apiKey
                   putStr "Past events: "
                   case response of
                     Left e  -> print e
                     Right r -> print $ pastEvents r
                   putStrLn ""
  where pastEvents = mapM (getContent <=< lookupChild "url") <=< lookupChildren "event" <=< lookupChild "events" <=< wrap

getPersonalTags :: IO ()
getPersonalTags = do response <- User.getPersonalTags (User "mokele") (Tag "rock") (TaggingType "artist") Nothing (Just $ Limit 10) apiKey
                     putStr "Personal tags: "
                     case response of
                       Left e  -> print e
                       Right r -> print $ personalTags r
                     putStrLn ""
  where personalTags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists" <=< lookupChild "taggings" <=< wrap

getPlaylists :: IO ()
getPlaylists = do response <- User.getPlaylists (User "mokele") apiKey
                  putStr "Playlists: "
                  case response of
                    Left e  -> print e
                    Right r -> print $ playlists r
                  putStrLn ""
  where playlists = mapM (getContent <=< lookupChild "title") <=< lookupChildren "playlist" <=< lookupChild "playlists" <=< wrap

getRecentStations :: APIKey -> SessionKey -> IO ()
getRecentStations apiKey sessionKey = do response <- User.getRecentStations (User "liblastfm") Nothing (Just $ Limit 10) apiKey sessionKey
                                         putStr "Recent stations: "
                                         case response of
                                           Left e  -> print e
                                           Right r -> print $ recentStations r
                                         putStrLn ""
  where recentStations = mapM (getContent <=< lookupChild "name") <=< lookupChildren "station" <=< lookupChild "recentstations" <=< wrap

getRecentTracks :: IO ()
getRecentTracks = do response <- User.getRecentTracks (User "smpcln") Nothing (Just $ Limit 10) Nothing Nothing apiKey
                     putStr "Recent tracks: "
                     case response of
                       Left e  -> print e
                       Right r -> print $ recentTracks r
                     putStrLn ""
  where recentTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "recenttracks" <=< wrap

getRecommendedArtists :: APIKey -> SessionKey -> IO ()
getRecommendedArtists apiKey sessionKey = do response <- User.getRecommendedArtists Nothing (Just $ Limit 10) apiKey sessionKey
                                             putStr "Recommended artists: "
                                             case response of
                                               Left e  -> print e
                                               Right r -> print $ recommendedArtists r
                                             putStrLn ""
  where recommendedArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "recommendations" <=< wrap

getRecommendedEvents :: APIKey -> SessionKey -> IO ()
getRecommendedEvents apiKey sessionKey = do response <- User.getRecommendedEvents Nothing (Just $ Limit 10) apiKey sessionKey
                                            putStr "Recommended events: "
                                            case response of
                                              Left e  -> print e
                                              Right r -> print $ recommendedEvents r
                                            putStrLn ""
  where recommendedEvents = mapM (getContent <=< lookupChild "url") <=< lookupChildren "event" <=< lookupChild "events" <=< wrap

getShouts :: IO ()
getShouts = do response <- User.getShouts (User "smpcln") Nothing (Just $ Limit 1) apiKey
               putStr "Shouts: "
               case response of
                 Left e  -> print e
                 Right r -> print $ shouts r
               putStrLn ""
  where shouts = mapM (getContent <=< lookupChild "body") <=< lookupChildren "shout" <=< lookupChild "shouts" <=< wrap

getTopAlbums :: IO ()
getTopAlbums = do response <- User.getTopAlbums (User "smpcln") Nothing Nothing (Just $ Limit 5) apiKey
                  putStr "Top albums: "
                  case response of
                    Left e  -> print e
                    Right r -> print $ topAlbums r
                  putStrLn ""
  where topAlbums = mapM (getContent <=< lookupChild "name" <=< lookupChild "artist") <=< lookupChildren "album" <=< lookupChild "topalbums" <=< wrap

getTopArtists :: IO ()
getTopArtists = do response <- User.getTopArtists (User "smpcln") Nothing Nothing (Just $ Limit 5) apiKey
                   putStr "Top artists: "
                   case response of
                     Left e  -> print e
                     Right r -> print $ topArtists r
                   putStrLn ""
  where topArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists" <=< wrap

getTopTags :: IO ()
getTopTags = do response <- User.getTopTags (User "smpcln") (Just $ Limit 10) apiKey
                putStr "Top tags: "
                case response of
                  Left e  -> print e
                  Right r -> print $ topTags r
                putStrLn ""
  where topTags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "toptags" <=< wrap

getTopTracks :: IO ()
getTopTracks = do response <- User.getTopTracks (User "smpcln") Nothing Nothing (Just $ Limit 10) apiKey
                  putStr "Top tracks: "
                  case response of
                    Left e  -> print e
                    Right r -> print $ topTracks r
                  putStrLn ""
  where topTracks = mapM (getContent <=< lookupChild "url") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

getWeeklyAlbumChart :: IO ()
getWeeklyAlbumChart = do response <- User.getWeeklyAlbumChart (User "rj") Nothing Nothing apiKey
                         putStr "Weekly album chart: "
                         case response of
                           Left e  -> print e
                           Right r -> print $ weeklyAlbumChart r
                         putStrLn ""
  where weeklyAlbumChart = mapM (getContent <=< lookupChild "url") <=< lookupChildren "album" <=< lookupChild "weeklyalbumchart" <=< wrap

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = do response <- User.getWeeklyArtistChart (User "rj") Nothing Nothing apiKey
                          putStr "Weekly artist chart: "
                          case response of
                            Left e  -> print e
                            Right r -> print $ weeklyArtistChart r
                          putStrLn ""
  where weeklyArtistChart = mapM (getContent <=< lookupChild "url") <=< lookupChildren "artist" <=< lookupChild "weeklyartistchart" <=< wrap

getWeeklyChartList :: IO ()
getWeeklyChartList = do response <- User.getWeeklyChartList (User "rj") apiKey
                        putStr "Weekly chart list: "
                        case response of
                          Left e  -> print e
                          Right r -> print $ take 10 r
                        putStrLn ""

getWeeklyTrackChart :: IO ()
getWeeklyTrackChart = do response <- User.getWeeklyTrackChart (User "rj") Nothing Nothing apiKey
                         putStr "Weekly track chart: "
                         case response of
                           Left e  -> print e
                           Right r -> print $ weeklyTrackChart r
                         putStrLn ""
  where weeklyTrackChart = mapM (getContent <=< lookupChild "url") <=< lookupChildren "track" <=< lookupChild "weeklytrackchart" <=< wrap

shout :: APIKey -> SessionKey -> IO ()
shout apiKey sessionKey = do response <- User.shout (User "liblastfm") (Message "test message") apiKey sessionKey
                             case response of
                               Left e   -> print e
                               Right () -> return ()

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
           getRecentTracks
           getShouts
           getTopAlbums
           getTopArtists
           getTopTags
           getTopTracks
           getWeeklyAlbumChart
           getWeeklyArtistChart
           getWeeklyChartList
           getWeeklyTrackChart
           (apiKey, sessionKey, secret) <- getConfig ".lastfm.conf"
           withSecret secret $ do getRecentStations apiKey sessionKey
                                  getRecommendedArtists apiKey sessionKey
                                  getRecommendedEvents apiKey sessionKey
                                  shout apiKey sessionKey

