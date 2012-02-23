module EPlaylist (common, auth) where

import Control.Arrow ((|||))

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Playlist as Playlist

addTrack :: APIKey -> SessionKey -> IO ()
addTrack ak sk = Playlist.addTrack (Playlist 10298486) (Artist "Apoptose") (Track "Horizont") ak sk >>= print ||| const (return ())

create :: APIKey -> SessionKey -> IO ()
create ak sk = Playlist.create (Just (Title "Awesome playlist")) Nothing ak sk >>= print ||| const (return ())

common :: IO ()
common = return ()

auth :: APIKey -> SessionKey -> IO ()
auth ak sk = do addTrack ak sk
                create ak sk
