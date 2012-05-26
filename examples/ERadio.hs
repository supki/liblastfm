module ERadio (common, auth) where

import Control.Monad ((<=<))

import Network.Lastfm
import qualified Network.Lastfm.XML.Radio as Radio

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

tune :: APIKey -> SessionKey -> Secret -> IO ()
tune ak sk s = parse r f "Tune"
  where r = Radio.tune Nothing (Station "lastfm://artist/Merzbow/similarartists") ak sk s
        f = fmap return . content <=< tag "url" <=< tag "station"

getPlaylist :: APIKey -> SessionKey -> Secret -> IO ()
getPlaylist ak sk s = parse r f "Playlist"
  where r = Radio.getPlaylist Nothing Nothing Nothing M2 B64 ak sk s
        f = mapM (content <=< tag "title") <=< tags "track" <=< tag "trackList" <=< tag "playlist"

search :: IO ()
search = parse r f "Dubstep stations"
  where r = Radio.search (Name "dubstep") apiKey
        f = mapM (content <=< tag "name") <=< tags "station" <=< tag "stations"

common :: IO ()
common = do search

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth ak sk s = do tune ak sk s
                  getPlaylist ak sk s
