#!/usr/bin/env runhaskell

import Control.Monad ((<=<))

import Network.Lastfm.Core
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Geo as Geo

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getEventsExample :: IO ()
getEventsExample = do response <- Geo.getEvents Nothing Nothing (Just (Location "Moscow")) Nothing Nothing (Just (Limit 5)) apiKey
                      putStr "First 5 Moscow events: "
                      case response of
                        Left e  -> print e
                        Right r -> print (events r)
  where events = mapM (getContent <=< lookupChild "id") <=< lookupChildren "event" <=< lookupChild "events"

getMetroArtistChartExample :: IO ()
getMetroArtistChartExample = do response <- Geo.getMetroArtistChart (Country "Russia") (Metro "Saint Petersburg") Nothing Nothing apiKey
                                putStr "Top Saint Petersburg artists: "
                                case response of
                                  Left e  -> print e
                                  Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists"

getMetroHypeArtistChartExample :: IO ()
getMetroHypeArtistChartExample = do response <- Geo.getMetroHypeArtistChart (Country "Russia") (Metro "Moscow") Nothing Nothing apiKey
                                    putStr "Top Moscow Hype artists: "
                                    case response of
                                      Left e  -> print e
                                      Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists"

getMetroHypeTrackChartExample :: IO ()
getMetroHypeTrackChartExample = do response <- Geo.getMetroHypeTrackChart (Country "Russia") (Metro "Ufa") Nothing Nothing apiKey
                                   putStr "Top Ufa Hype tracks: "
                                   case response of
                                     Left e  -> print e
                                     Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks"

getMetroTrackChartExample :: IO ()
getMetroTrackChartExample = do response <- Geo.getMetroTrackChart (Country "Ukraine") (Metro "Kyiv") Nothing Nothing apiKey
                               putStr "Top Kiyv tracks: "
                               case response of
                                 Left e  -> print e
                                 Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks"

getMetroUniqueArtistChartExample :: IO ()
getMetroUniqueArtistChartExample = do response <- Geo.getMetroUniqueArtistChart (Country "Belarus") (Metro "Minsk") Nothing Nothing apiKey
                                      putStr "Top unique Minsk artists: "
                                      case response of
                                        Left e  -> print e
                                        Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists"

getMetroUniqueTrackChartExample :: IO ()
getMetroUniqueTrackChartExample = do response <- Geo.getMetroUniqueTrackChart (Country "Ukraine") (Metro "Odesa") Nothing Nothing apiKey
                                     putStr "Top unique Odesa tracks: "
                                     case response of
                                       Left e  -> print e
                                       Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks"

getMetroWeeklyChartlistExample :: IO ()
getMetroWeeklyChartlistExample = do response <- Geo.getMetroWeeklyChartlist (Metro "Moscow") apiKey
                                    putStr "First 10 Moscow chartlist intervals: "
                                    case response of
                                      Left e  -> print e
                                      Right r -> print (take 10 r)

getMetrosExample :: IO ()
getMetrosExample = do response <- Geo.getMetros (Just (Country "Russia")) apiKey
                      putStr "All Russia metros: "
                      case response of
                        Left e  -> print e
                        Right r -> print (metros r)
  where metros = mapM (getContent <=< lookupChild "name") <=< lookupChildren "metro" <=< lookupChild "metros"

getTopArtistsExample :: IO ()
getTopArtistsExample = do response <- Geo.getTopArtists (Country "Belarus") Nothing (Just (Limit 10)) apiKey
                          putStr "Top 10 Belarus artists: "
                          case response of
                            Left e  -> print e
                            Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "topartists"

getTopTracksExample :: IO ()
getTopTracksExample = do response <- Geo.getTopTracks (Country "Ukraine") Nothing Nothing (Just (Limit 10)) apiKey
                         putStr "Top 10 Ukraine tracks: "
                         case response of
                           Left e  -> print e
                           Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks"

main :: IO ()
main = do getEventsExample
          getMetroArtistChartExample
          getMetroHypeArtistChartExample
          getMetroHypeTrackChartExample
          getMetroTrackChartExample
          getMetroUniqueArtistChartExample
          getMetroUniqueTrackChartExample
          getMetroWeeklyChartlistExample
          getMetrosExample
          getTopArtistsExample
          getTopTracksExample
