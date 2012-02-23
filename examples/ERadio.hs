module ERadio (common, auth) where

import Control.Monad ((<=<))

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Radio as Radio

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

tune :: APIKey -> SessionKey -> IO ()
tune ak sk = parse r f "Tune"
  where r = Radio.tune Nothing (Station "lastfm://artist/Merzbow/similarartists") ak sk
        f = fmap return . content <=< tag "url" <=< tag "station"

getPlaylist :: APIKey -> SessionKey -> IO ()
getPlaylist ak sk = parse r f "Playlist"
  where r = Radio.getPlaylist Nothing Nothing Nothing (Multiplier 2.0) (Bitrate 64) ak sk
        f = mapM (content <=< tag "title") <=< tags "track" <=< tag "trackList" <=< tag "playlist"

search :: IO ()
search = parse r f "Dubstep stations"
  where r = Radio.search (Name "dubstep") apiKey
        f = mapM (content <=< tag "name") <=< tags "station" <=< tag "stations"

common :: IO ()
common = do search

auth :: APIKey -> SessionKey -> IO ()
auth ak sk = do tune ak sk
                getPlaylist ak sk
