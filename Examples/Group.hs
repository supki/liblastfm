#!/usr/bin/env runhaskell

import Control.Monad ((<=<))

import Network.Lastfm.Core
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Group as Group

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
group = Group "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"

getHype :: IO ()
getHype = do response <- Group.getHype group apiKey
             putStr "Weekly top artists mbids: "
             case response of
               Left e  -> print e
               Right r -> print (mbids r)
  where mbids = mapM (getContent <=< lookupChild "mbid") <=< lookupChildren "artist" <=< lookupChild "weeklyartistchart"

getMembers :: IO ()
getMembers = do response <- Group.getMembers group Nothing (Just (Limit 10)) apiKey
                putStr "Top 10 members: "
                case response of
                  Left e  -> print e
                  Right r -> print (members r)
  where members = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "members"

getWeeklyAlbumChart :: IO ()
getWeeklyAlbumChart = do response <- Group.getWeeklyAlbumChart group Nothing Nothing apiKey
                         putStr "Playcount list: "
                         case response of
                           Left e  -> print e
                           Right r -> print (playcounts r)
  where playcounts = mapM (getContent <=< lookupChild "playcount") <=< lookupChildren "album" <=< lookupChild "weeklyalbumchart"

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = do response <- Group.getWeeklyArtistChart group Nothing Nothing apiKey
                          putStr "Artist list: "
                          case response of
                            Left e  -> print e
                            Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "weeklyartistchart"

getWeeklyChartList :: IO ()
getWeeklyChartList = do response <- Group.getWeeklyChartList group apiKey
                        putStr "First 10 chartlist intervals: "
                        case response of
                          Left e  -> print e
                          Right r -> print (take 10 r)

getWeeklyTrackChart :: IO ()
getWeeklyTrackChart = do response <- Group.getWeeklyTrackChart group Nothing Nothing apiKey
                         putStr "Url list: "
                         case response of
                           Left e  -> print e
                           Right r -> print (urls r)
  where urls = mapM (getContent <=< lookupChild "url") <=< lookupChildren "track" <=< lookupChild "weeklytrackchart"

main :: IO ()
main = do getHype
          getMembers
          getWeeklyAlbumChart
          getWeeklyArtistChart
          getWeeklyChartList
          getWeeklyTrackChart
