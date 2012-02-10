module EGroup (common, auth) where

import Control.Arrow ((***), (&&&))
import Control.Monad ((<=<), join)
import Data.Maybe (fromMaybe)

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Group as Group

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
group = Group "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"

getHype :: IO ()
getHype = do response <- Group.getHype group apiKey
             putStr "Weekly top artists mbids: "
             case response of
               Left e  -> print e
               Right r -> print (mbids r)
             putStrLn ""
  where mbids = mapM (getContent <=< lookupChild "mbid") <=< lookupChildren "artist" <=< lookupChild "weeklyartistchart" <=< wrap

getMembers :: IO ()
getMembers = do response <- Group.getMembers group Nothing (Just (Limit 10)) apiKey
                putStr "Top 10 members: "
                case response of
                  Left e  -> print e
                  Right r -> print (members r)
                putStrLn ""
  where members = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "members" <=< wrap

getWeeklyAlbumChart :: IO ()
getWeeklyAlbumChart = do response <- Group.getWeeklyAlbumChart group Nothing Nothing apiKey
                         putStr "Playcount list: "
                         case response of
                           Left e  -> print e
                           Right r -> print (playcounts r)
                         putStrLn ""
  where playcounts = mapM (getContent <=< lookupChild "playcount") <=< lookupChildren "album" <=< lookupChild "weeklyalbumchart" <=< wrap

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = do response <- Group.getWeeklyArtistChart group Nothing Nothing apiKey
                          putStr "Artist list: "
                          case response of
                            Left e  -> print e
                            Right r -> print (artists r)
                          putStrLn ""
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "weeklyartistchart" <=< wrap

getWeeklyChartList :: IO ()
getWeeklyChartList = do response <- Group.getWeeklyChartList group apiKey
                        putStr "First 10 chartlist intervals: "
                        case response of
                          Left e  -> print e
                          Right r -> print (intervals r)
  where intervals = take 10 . map (join (***) ((read :: String -> Integer) . fromMaybe "0") . (getAttribute "from" &&& getAttribute "to")) . fromMaybe [] . (lookupChildren "chart" <=< lookupChild "weeklychartlist" <=< wrap)

getWeeklyTrackChart :: IO ()
getWeeklyTrackChart = do response <- Group.getWeeklyTrackChart group Nothing Nothing apiKey
                         putStr "Url list: "
                         case response of
                           Left e  -> print e
                           Right r -> print (urls r)
                         putStrLn ""
  where urls = mapM (getContent <=< lookupChild "url") <=< lookupChildren "track" <=< lookupChild "weeklytrackchart" <=< wrap

common :: IO ()
common = do getHype
            getMembers
            getWeeklyAlbumChart
            getWeeklyArtistChart
            getWeeklyChartList
            getWeeklyTrackChart

auth :: APIKey -> SessionKey -> IO ()
auth _ _ = return ()
