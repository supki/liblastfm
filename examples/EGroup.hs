module EGroup (common, auth) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<), liftM2)

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Group as Group

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
group = Group "People with no social lives that listen to more music than is healthy who are slightly scared of spiders and can never seem to find a pen"

getHype :: IO ()
getHype = parse r f "Weekly top artists mbids"
  where r = Group.getHype group apiKey
        f = mapM (content <=< tag "mbid") <=< tags "artist" <=< tag "weeklyartistchart"

getMembers :: IO ()
getMembers = parse r f "Top 10 members"
  where r = Group.getMembers group Nothing (Just (Limit 10)) apiKey
        f = mapM (content <=< tag "name") <=< tags "user" <=< tag "members"

getWeeklyAlbumChart :: IO ()
getWeeklyAlbumChart = parse r f "Playcount list"
  where r = Group.getWeeklyAlbumChart group Nothing Nothing apiKey
        f = mapM (content <=< tag "playcount") <=< tags "album" <=< tag "weeklyalbumchart"

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = parse r f "Artist list"
  where r = Group.getWeeklyArtistChart group Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "weeklyartistchart"

getWeeklyChartList :: IO ()
getWeeklyChartList = parse r f "First 10 chartlist intervals"
  where r = Group.getWeeklyChartList group apiKey
        f = mapM (pretty . getFromToAttributes) <=< fmap (take 10) . tags "chart" <=< tag "weeklychartlist"
        pretty = uncurry $ liftM2 $ \from to -> "(" ++ from ++ "," ++ to ++ ")"
        getFromToAttributes = getAttribute "from" &&& getAttribute "to"

getWeeklyTrackChart :: IO ()
getWeeklyTrackChart = parse r f "Url list"
  where r = Group.getWeeklyTrackChart group Nothing Nothing apiKey
        f = mapM (content <=< tag "url") <=< tags "track" <=< tag "weeklytrackchart"

common :: IO ()
common = do getHype
            getMembers
            getWeeklyAlbumChart
            getWeeklyArtistChart
            getWeeklyChartList
            getWeeklyTrackChart

auth :: APIKey -> SessionKey -> IO ()
auth _ _ = return ()
