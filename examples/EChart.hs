module EChart (common, auth) where

import Control.Monad ((<=<))

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Chart as Chart

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getHypedArtists :: IO ()
getHypedArtists = parse r f "Top 8 hyped artists"
  where r = Chart.getHypedArtists Nothing (Just (Limit 8)) apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "artists"

getHypedTracks :: IO ()
getHypedTracks = parse r f "Top 6 hyped artists"
  where r = Chart.getHypedTracks Nothing (Just (Limit 6)) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "tracks"

getLovedTracks :: IO ()
getLovedTracks = parse r f "Top 9 most loved tracks"
  where r = Chart.getLovedTracks Nothing (Just (Limit 9)) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "tracks"

getTopArtists :: IO ()
getTopArtists = parse r f "Top 4 artists"
  where r = Chart.getTopArtists Nothing (Just (Limit 4)) apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "artists"

getTopTags :: IO ()
getTopTags = parse r f "Top 6 tags"
  where r = Chart.getTopTags Nothing (Just (Limit 6)) apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "tags"

getTopTracks :: IO ()
getTopTracks = parse r f "Top 7 tracks"
  where r = Chart.getTopTracks Nothing (Just (Limit 7)) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "tracks"

common :: IO ()
common = do getHypedArtists
            getHypedTracks
            getLovedTracks
            getTopArtists
            getTopTags
            getTopTracks

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth _ _ _ = return ()
