module ETag (common, auth) where

import Control.Monad ((<=<))

import Network.Lastfm.Response ()
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Tag as Tag

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getInfo :: IO ()
getInfo = do response <- Tag.getInfo (Tag "depressive") Nothing apiKey
             putStr "Info: "
             case response of
               Left e  -> print e
               Right r -> print $ info r
             putStrLn ""
  where info = getContent <=< lookupChild "taggings" <=< lookupChild "tag" <=< wrap

getSimilar :: IO ()
getSimilar = do response <- Tag.getSimilar (Tag "depressive") apiKey
                putStr "Similar: "
                case response of
                  Left e  -> print e
                  Right r -> print $ similar r
                putStrLn ""
  where similar = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "similartags" <=< wrap

getTopAlbums :: IO ()
getTopAlbums = do response <- Tag.getTopAlbums (Tag "depressive") Nothing (Just $ Limit 10) apiKey
                  putStr "Top albums: "
                  case response of
                    Left e  -> print e
                    Right r -> print $ topAlbums r
                  putStrLn ""
  where topAlbums = mapM (getContent <=< lookupChild "url") <=< lookupChildren "album" <=< lookupChild "topalbums" <=< wrap

getTopArtists :: IO ()
getTopArtists = do response <- Tag.getTopArtists (Tag "depressive") Nothing (Just $ Limit 10) apiKey
                   putStr "Top artists: "
                   case response of
                     Left e  -> print e
                     Right r -> print $ topArtists r
                   putStrLn ""
  where topArtists = mapM (getContent <=< lookupChild "url") <=< lookupChildren "artist" <=< lookupChild "topartists" <=< wrap

getTopTags :: IO ()
getTopTags = do response <- Tag.getTopTags apiKey
                putStr "Top tags: "
                case response of
                  Left e  -> print e
                  Right r -> print $ topTags r
                putStrLn ""
  where topTags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "toptags" <=< wrap

getTopTracks :: IO ()
getTopTracks = do response <- Tag.getTopTracks (Tag "depressive") Nothing (Just $ Limit 10) apiKey
                  putStr "Top tracks: "
                  case response of
                    Left e  -> print e
                    Right r -> print $ topTracks r
                  putStrLn ""
  where topTracks = mapM (getContent <=< lookupChild "url") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = do response <- Tag.getWeeklyArtistChart (Tag "depressive") Nothing Nothing (Just $ Limit 10) apiKey
                          putStr "Weekly artist chart: "
                          case response of
                            Left e  -> print e
                            Right r -> print $ weeklyArtistChart r
                          putStrLn ""
  where weeklyArtistChart = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "weeklyartistchart" <=< wrap

getWeeklyChartList :: IO ()
getWeeklyChartList = do response <- Tag.getWeeklyChartList (Tag "depressive") apiKey
                        putStr "Weekly chart list: "
                        case response of
                          Left e  -> print e
                          Right r -> print $ take 10 r
                        putStrLn ""

search :: IO ()
search = do response <- Tag.search (Tag "depressive") Nothing (Just $ Limit 10) apiKey
            putStr "search: "
            case response of
              Left e  -> print e
              Right r -> print $ search' r
            putStrLn ""
  where search' = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "tagmatches" <=< lookupChild "results" <=< wrap

common :: IO ()
common = do getInfo
            getSimilar
            getTopAlbums
            getTopArtists
            getTopTags -- should be fixed
            getTopTracks
            getWeeklyArtistChart -- should be fixed
            getWeeklyChartList
            search

auth :: APIKey -> SessionKey -> IO ()
auth _ _ = return ()
