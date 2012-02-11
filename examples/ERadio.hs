module ERadio (common, auth) where

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Radio as Radio

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

tune :: APIKey -> SessionKey -> IO ()
tune spiKey sessionKey = do response <- Radio.tune Nothing (Station "lastfm://artist/Merzbow/similarartists") apiKey sessionKey
                            putStr "Tune: "
                            case response of
                              Left e  -> print e
                              Right r -> print $ stationUrl r
                            putStrLn ""
  where stationUrl = getContent <=< lookupChild "url" <=< lookupChild "station" <=< wrap

getPlaylist :: APIKey -> SessionKey -> IO ()
getPlaylist apiKey sessionKey = do response <- Radio.getPlaylist Nothing Nothing Nothing (Multiplier 2.0) (Bitrate 64) apiKey sessionKey
                                   putStr "Playlist: "
                                   case response of
                                     Left e  -> print e
                                     Right r -> print $ titles r
                                   putStrLn ""
  where titles = mapM (getContent <=< lookupChild "title") <=< lookupChildren "track" <=< lookupChild "trackList" <=< lookupChild "playlist" <=< wrap

search :: IO ()
search = do response <- Radio.search (Name "dubstep") apiKey
            putStr "Dubstep stations: "
            case response of
              Left e  -> print e
              Right r -> print $ stations r
            putStrLn ""
  where stations = mapM (getContent <=< lookupChild "name") <=< lookupChildren "station" <=< lookupChild "stations" <=< wrap

common :: IO ()
common = do search

auth :: APIKey -> SessionKey -> IO ()
auth apiKey sessionKey = do tune apiKey sessionKey
                            getPlaylist apiKey sessionKey
