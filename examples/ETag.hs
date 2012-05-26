module ETag (common, auth) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<), liftM2)

import Network.Lastfm.Types
import qualified Network.Lastfm.XML.Tag as Tag

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getInfo :: IO ()
getInfo = parse r f "Info"
  where r = Tag.getInfo (Tag "depressive") Nothing apiKey
        f = fmap return . content <=< tag "taggings" <=< tag "tag"

getSimilar :: IO ()
getSimilar = parse r f "Similar"
  where r = Tag.getSimilar (Tag "depressive") apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "similartags"

getTopAlbums :: IO ()
getTopAlbums = parse r f "Top albums"
  where r = Tag.getTopAlbums (Tag "depressive") Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "url") <=< tags "album" <=< tag "topalbums"

getTopArtists :: IO ()
getTopArtists = parse r f "Top artists"
  where r = Tag.getTopArtists (Tag "depressive") Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "url") <=< tags "artist" <=< tag "topartists"

getTopTags :: IO ()
getTopTags = parse r f "Top tags"
  where r = Tag.getTopTags apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "toptags"

getTopTracks :: IO ()
getTopTracks = parse r f "Top tracks"
  where r = Tag.getTopTracks (Tag "depressive") Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "url") <=< tags "track" <=< tag "toptracks"

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = parse r f "Weekly artist chart"
  where r = Tag.getWeeklyArtistChart (Tag "depressive") Nothing Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "weeklyartistchart"

getWeeklyChartList :: IO ()
getWeeklyChartList = parse r f "Weekly chart list"
  where r = Tag.getWeeklyChartList (Tag "depressive") apiKey
        f = mapM (pretty . getFromToAttributes) <=< fmap (take 10) . tags "chart" <=< tag "weeklychartlist"
        pretty = uncurry $ liftM2 $ \from to -> "(" ++ from ++ "," ++ to ++ ")"
        getFromToAttributes = getAttribute "from" &&& getAttribute "to"

search :: IO ()
search = parse r f "Search"
  where r = Tag.search (Tag "depressive") Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "tagmatches" <=< tag "results"

common :: IO ()
common = do getInfo
            getSimilar
            getTopAlbums
            getTopArtists
            getTopTags -- should be fixed
            getTopTracks
            getWeeklyArtistChart
            getWeeklyChartList
            search

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth _ _ _ = return ()
