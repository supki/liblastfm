module EGeo (common, auth) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<), liftM2)

import Network.Lastfm.Types
import qualified Network.Lastfm.XML.Geo as Geo

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getEvents :: IO ()
getEvents = parse r f "First 5 Moscow events"
  where r = Geo.getEvents Nothing Nothing (Just (Location "Moscow")) Nothing Nothing (Just (Limit 5)) apiKey
        f = mapM (content <=< tag "id") <=< tags "event" <=< tag "events"

getMetroArtistChart :: IO ()
getMetroArtistChart = parse r f "Top Sain Petesburg artists"
  where r = Geo.getMetroArtistChart (Country "Russia") (Metro "Saint Petersburg") Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "topartists"

getMetroHypeArtistChart :: IO ()
getMetroHypeArtistChart = parse r f "Top Moscow Hype artists"
  where r = Geo.getMetroHypeArtistChart (Country "Russia") (Metro "Moscow") Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "topartists"

getMetroHypeTrackChart :: IO ()
getMetroHypeTrackChart = parse r f "Top Ufa Hype tracks"
  where r = Geo.getMetroHypeTrackChart (Country "Russia") (Metro "Ufa") Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "toptracks"

getMetroTrackChart :: IO ()
getMetroTrackChart = parse r f "Top Kiyv tracks"
  where r = Geo.getMetroTrackChart (Country "Ukraine") (Metro "Kyiv") Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "toptracks"

getMetroUniqueArtistChart :: IO ()
getMetroUniqueArtistChart = parse r f "Top unique Minsk artists"
  where r = Geo.getMetroUniqueArtistChart (Country "Belarus") (Metro "Minsk") Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "topartists"

getMetroUniqueTrackChart :: IO ()
getMetroUniqueTrackChart = parse r f "Top unique Odesa tracks"
  where r = Geo.getMetroUniqueTrackChart (Country "Ukraine") (Metro "Odesa") Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "toptracks"

getMetroWeeklyChartlist :: IO ()
getMetroWeeklyChartlist = parse r f "First 10 Moscow chartlist intervals"
  where r = Geo.getMetroWeeklyChartlist (Metro "Moscow") apiKey
        f = mapM (pretty . getFromToAttributes) <=< fmap (take 10) . tags "chart" <=< tag "weeklychartlist"
        pretty = uncurry $ liftM2 $ \from to -> "(" ++ from ++ "," ++ to ++ ")"
        getFromToAttributes = getAttribute "from" &&& getAttribute "to"

getMetros :: IO ()
getMetros = parse r f "All Russia metros"
  where r = Geo.getMetros (Just (Country "Russia")) apiKey
        f = mapM (content <=< tag "name") <=< tags "metro" <=< tag "metros"

getTopArtists :: IO ()
getTopArtists = parse r f "Top 10 Belarus artists"
  where r = Geo.getTopArtists (Country "Belarus") Nothing (Just (Limit 10)) apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "topartists"

getTopTracks :: IO ()
getTopTracks = parse r f "Top 10 Ukraine tracks"
  where r = Geo.getTopTracks (Country "Ukraine") Nothing Nothing (Just (Limit 10)) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "toptracks"

common :: IO ()
common = do getEvents
            getMetroArtistChart
            getMetroHypeArtistChart
            getMetroHypeTrackChart
            getMetroTrackChart
            getMetroUniqueArtistChart
            getMetroUniqueTrackChart
            getMetroWeeklyChartlist
            getMetros
            getTopArtists
            getTopTracks

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth _ _ _ = return ()
