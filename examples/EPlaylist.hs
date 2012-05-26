module EPlaylist (common, auth) where

import Control.Arrow ((|||))

import Network.Lastfm
import qualified Network.Lastfm.XML.Playlist as Playlist

addTrack :: APIKey -> SessionKey -> Secret -> IO ()
addTrack ak sk s = Playlist.addTrack (Playlist 10298486) (Artist "Apoptose") (Track "Horizont") ak sk s >>= print ||| const (return ())

create :: APIKey -> SessionKey -> Secret -> IO ()
create ak sk s = Playlist.create (Just (Title "Awesome playlist")) Nothing ak sk s >>= print ||| const (return ())

common :: IO ()
common = return ()

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth ak sk s = do addTrack ak sk s
                  create ak sk s
