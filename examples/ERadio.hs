module ERadio (common, auth) where

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Radio as Radio

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

tune :: APIKey -> SessionKey -> IO ()
tune = undefined

getPlaylist :: APIKey -> SessionKey -> IO ()
getPlaylist = undefined

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
