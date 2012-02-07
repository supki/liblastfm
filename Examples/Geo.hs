#!/usr/bin/env runhaskell

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Geo as Geo

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getEvents :: IO ()
getEvents = do response <- Geo.getEvents Nothing Nothing (Just (Location "Moscow")) Nothing Nothing (Just (Limit 5)) apiKey
               putStr "First 5 Moscow events: "
               case response of
                 Left e  -> print e
                 Right r -> print (events r)
  where events = mapM (getContent <=< lookupChild "id") <=< lookupChildren "event" <=< lookupChild "events" <=< wrap

getMetroArtistChart :: IO ()
getMetroArtistChart = do response <- Geo.getMetroArtistChart (Country "Russia") (Metro "Saint Petersburg") Nothing Nothing apiKey
                         putStr "Top Saint Petersburg artists: "
                         case response of
                           Left e  -> print e
                           Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists" <=< wrap

getMetroHypeArtistChart :: IO ()
getMetroHypeArtistChart = do response <- Geo.getMetroHypeArtistChart (Country "Russia") (Metro "Moscow") Nothing Nothing apiKey
                             putStr "Top Moscow Hype artists: "
                             case response of
                               Left e  -> print e
                               Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists" <=< wrap

getMetroHypeTrackChart :: IO ()
getMetroHypeTrackChart = do response <- Geo.getMetroHypeTrackChart (Country "Russia") (Metro "Ufa") Nothing Nothing apiKey
                            putStr "Top Ufa Hype tracks: "
                            case response of
                              Left e  -> print e
                              Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

getMetroTrackChart :: IO ()
getMetroTrackChart = do response <- Geo.getMetroTrackChart (Country "Ukraine") (Metro "Kyiv") Nothing Nothing apiKey
                        putStr "Top Kiyv tracks: "
                        case response of
                          Left e  -> print e
                          Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

getMetroUniqueArtistChart :: IO ()
getMetroUniqueArtistChart = do response <- Geo.getMetroUniqueArtistChart (Country "Belarus") (Metro "Minsk") Nothing Nothing apiKey
                               putStr "Top unique Minsk artists: "
                               case response of
                                 Left e  -> print e
                                 Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists" <=< wrap

getMetroUniqueTrackChart :: IO ()
getMetroUniqueTrackChart = do response <- Geo.getMetroUniqueTrackChart (Country "Ukraine") (Metro "Odesa") Nothing Nothing apiKey
                              putStr "Top unique Odesa tracks: "
                              case response of
                                Left e  -> print e
                                Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

{-
getMetroWeeklyChartlist :: IO ()
getMetroWeeklyChartlist = do response <- Geo.getMetroWeeklyChartlist (Metro "Moscow") apiKey
                             putStr "First 10 Moscow chartlist intervals: "
                             case response of
                               Left e  -> print e
                               Right r -> print (take 10 r)
-}

getMetros :: IO ()
getMetros = do response <- Geo.getMetros (Just (Country "Russia")) apiKey
               putStr "All Russia metros: "
               case response of
                 Left e  -> print e
                 Right r -> print (metros r)
  where metros = mapM (getContent <=< lookupChild "name") <=< lookupChildren "metro" <=< lookupChild "metros" <=< wrap

getTopArtists :: IO ()
getTopArtists = do response <- Geo.getTopArtists (Country "Belarus") Nothing (Just (Limit 10)) apiKey
                   putStr "Top 10 Belarus artists: "
                   case response of
                     Left e  -> print e
                     Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists" <=< wrap

getTopTracks :: IO ()
getTopTracks = do response <- Geo.getTopTracks (Country "Ukraine") Nothing Nothing (Just (Limit 10)) apiKey
                  putStr "Top 10 Ukraine tracks: "
                  case response of
                    Left e  -> print e
                    Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

main :: IO ()
main = do getEvents
          getMetroArtistChart
          getMetroHypeArtistChart
          getMetroHypeTrackChart
          getMetroTrackChart
          getMetroUniqueArtistChart
          getMetroUniqueTrackChart
          --getMetroWeeklyChartlist
          getMetros
          getTopArtists
          getTopTracks
