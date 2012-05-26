module EUser (common, auth) where

import Control.Arrow ((|||), (&&&))
import Control.Monad ((<=<), liftM2)

import Network.Lastfm
import qualified Network.Lastfm.XML.User as User

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getArtistTracks :: IO ()
getArtistTracks = parse r f "Artist tracks"
  where r = User.getArtistTracks (User "smpcln") (Artist "Dvar") Nothing Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "artisttracks"

getBannedTracks :: IO ()
getBannedTracks = parse r f "Banned artists"
  where r = User.getBannedTracks (User "smpcln") Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "bannedtracks"

getEvents :: IO ()
getEvents = parse r f "Events"
  where r = User.getEvents (User "mokele") Nothing (Just $ Limit 5) Nothing apiKey
        f = mapM (content <=< tag "url" <=< tag "venue") <=< tags "event" <=< tag "events"

getFriends :: IO ()
getFriends = parse r f "Friends"
  where r = User.getFriends (User "smpcln") Nothing Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "user" <=< tag "friends"

getPlayCount :: IO ()
getPlayCount = parse r f "Play count"
  where r = User.getInfo (Just (User "smpcln")) apiKey
        f = fmap return . content <=< tag "playcount" <=< tag "user"

getLovedTracks :: IO ()
getLovedTracks = parse r f "Loved tracks"
  where r = User.getLovedTracks (User "smpcln") Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "lovedtracks"

getNeighbours :: IO ()
getNeighbours = parse r f "Neighbours"
  where r = User.getNeighbours (User "smpcln") (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "user" <=< tag "neighbours"

getNewReleases :: IO ()
getNewReleases = parse r f "New releases"
  where r = User.getNewReleases (User "smpcln") Nothing apiKey
        f = mapM (content <=< tag "url") <=< tags "album" <=< tag "albums"

getPastEvents :: IO ()
getPastEvents = parse r f "Past events"
  where r = User.getPastEvents (User "mokele") Nothing (Just $ Limit 5) apiKey
        f = mapM (content <=< tag "url") <=< tags "event" <=< tag "events"

getPersonalTags :: IO ()
getPersonalTags = parse r f "Personal tags"
  where r = User.getPersonalTags (User "mokele") (Tag "rock") (TaggingType "artist") Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "artists" <=< tag "taggings"

getPlaylists :: IO ()
getPlaylists = parse r f "Playlists"
  where r = User.getPlaylists (User "mokele") apiKey
        f = mapM (content <=< tag "title") <=< tags "playlist" <=< tag "playlists"

getRecentStations :: APIKey -> SessionKey -> Secret -> IO ()
getRecentStations ak sk s = parse r f "Recent stations"
  where r = User.getRecentStations (User "liblastfm") Nothing (Just $ Limit 10) ak sk s
        f = mapM (content <=< tag "name") <=< tags "station" <=< tag "recentstations"

getRecentTracks :: IO ()
getRecentTracks = parse r f "Recent tracks"
  where r = User.getRecentTracks (User "smpcln") Nothing (Just $ Limit 10) Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "recenttracks"

getRecommendedArtists :: APIKey -> SessionKey -> Secret -> IO ()
getRecommendedArtists ak sk s = parse r f "Recommended artists"
  where r = User.getRecommendedArtists Nothing (Just $ Limit 10) ak sk s
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "recommendations"

getRecommendedEvents :: APIKey -> SessionKey -> Secret -> IO ()
getRecommendedEvents ak sk s = parse r f "Recommended events"
  where r = User.getRecommendedEvents Nothing (Just $ Limit 10) ak sk s
        f = mapM (content <=< tag "url") <=< tags "event" <=< tag "events"

getShouts :: IO ()
getShouts = parse r f "Shouts"
  where r = User.getShouts (User "smpcln") Nothing (Just $ Limit 1) apiKey
        f = mapM (content <=< tag "body") <=< tags "shout" <=< tag "shouts"

getTopAlbums :: IO ()
getTopAlbums = parse r f "Top albums"
  where r = User.getTopAlbums (User "smpcln") Nothing Nothing (Just $ Limit 5) apiKey
        f = mapM (content <=< tag "name" <=< tag "artist") <=< tags "album" <=< tag "topalbums"

getTopArtists :: IO ()
getTopArtists = parse r f "Top artists"
  where r = User.getTopArtists (User "smpcln") Nothing Nothing (Just $ Limit 5) apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "topartists"

getTopTags :: IO ()
getTopTags = parse r f "Top tags"
  where r = User.getTopTags (User "smpcln") (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "toptags"

getTopTracks :: IO ()
getTopTracks = parse r f "Top tracks"
  where r = User.getTopTracks (User "smpcln") Nothing Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "url") <=< tags "track" <=< tag "toptracks"

getWeeklyAlbumChart :: IO ()
getWeeklyAlbumChart = parse r f "Weekly album chart"
  where r = User.getWeeklyAlbumChart (User "rj") Nothing Nothing apiKey
        f = mapM (content <=< tag "url") <=< tags "album" <=< tag "weeklyalbumchart"

getWeeklyArtistChart :: IO ()
getWeeklyArtistChart = parse r f "Weekly artist chart"
  where r = User.getWeeklyArtistChart (User "rj") Nothing Nothing apiKey
        f = mapM (content <=< tag "url") <=< tags "artist" <=< tag "weeklyartistchart"

getWeeklyChartList :: IO ()
getWeeklyChartList = parse r f "Weekly chart list"
  where r = User.getWeeklyChartList (User "rj") apiKey
        f = mapM (pretty . getFromToAttributes) <=< fmap (take 10) . tags "chart" <=< tag "weeklychartlist"
        pretty = uncurry $ liftM2 $ \from to -> "(" ++ from ++ "," ++ to ++ ")"
        getFromToAttributes = getAttribute "from" &&& getAttribute "to"

getWeeklyTrackChart :: IO ()
getWeeklyTrackChart = parse r f "Weekly track chart"
  where r = User.getWeeklyTrackChart (User "rj") Nothing Nothing apiKey
        f = mapM (content <=< tag "url") <=< tags "track" <=< tag "weeklytrackchart"

shout :: APIKey -> SessionKey -> Secret -> IO ()
shout ak sk s = User.shout (User "liblastfm") (Message "test message") ak sk s >>= print ||| const (return ())

common :: IO ()
common = do getArtistTracks
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

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth ak sk s = do getRecentStations ak sk s
                  getRecommendedArtists ak sk s
                  getRecommendedEvents ak sk s
                  shout ak sk s

