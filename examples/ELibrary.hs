module ELibrary (common, auth) where

import Control.Arrow ((|||))
import Control.Monad ((<=<))

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Library as Library

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user = User "smpcln"

addAlbum :: APIKey -> SessionKey -> Secret -> IO ()
addAlbum ak sk s = Library.addAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") ak sk s >>= print ||| const (return ())

addArtist :: APIKey -> SessionKey -> Secret -> IO ()
addArtist ak sk s = Library.addArtist (Artist "Mobthrow") ak sk s >>= print ||| const (return ())

addTrack :: APIKey -> SessionKey -> Secret -> IO ()
addTrack ak sk s = Library.addTrack (Artist "Eminem") (Track "Kim") ak sk s >>= print ||| const (return ())

getAlbums :: IO ()
getAlbums = parse r f "Top 5 popular Burzum albums"
  where r = Library.getAlbums user (Just $ Artist "Burzum") Nothing (Just $ Limit 5) apiKey
        f = mapM (content <=< tag "name") <=< tags "album" <=< tag "albums"

getArtists :: IO ()
getArtists = parse r f "Top 7 popular artists playcounts"
  where r = Library.getArtists user Nothing (Just $ Limit 7) apiKey
        f = mapM (content <=< tag "name") <=< tags "artist" <=< tag "artists"

getTracks :: IO ()
getTracks = parse r f "First 4 Burzum tracks"
  where r = Library.getTracks user (Just $ Artist "Burzum") Nothing Nothing (Just $ Limit 4) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "tracks"

removeAlbum :: APIKey -> SessionKey -> Secret -> IO ()
removeAlbum ak sk s = Library.removeAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") ak sk s >>= print ||| const (return ())

removeArtist :: APIKey -> SessionKey -> Secret -> IO ()
removeArtist ak sk s = Library.removeArtist (Artist "Burzum") ak sk s >>= print ||| const (return ())

removeTrack :: APIKey -> SessionKey -> Secret -> IO ()
removeTrack ak sk s = Library.removeTrack (Artist "Eminem") (Track "Kim") ak sk s >>= print ||| const (return ())

removeScrobble :: APIKey -> SessionKey -> Secret -> IO ()
removeScrobble ak sk s = Library.removeScrobble (Artist "Gojira") (Track "Ocean") (Timestamp 1328905590) ak sk s >>= print ||| const (return ())

common :: IO ()
common = do getAlbums
            getArtists
            getTracks

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth ak sk s = do addAlbum ak sk s
                  addArtist ak sk s
                  addTrack ak sk s
                  removeAlbum ak sk s
                  removeArtist ak sk s
                  removeTrack ak sk s
                  removeScrobble ak sk s
