module ELibrary (common, auth) where

import Control.Arrow ((|||))
import Control.Monad ((<=<))

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Library as Library

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user = User "smpcln"

addAlbum :: APIKey -> SessionKey -> IO ()
addAlbum ak sk = Library.addAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") ak sk >>= print ||| const (return ())

addArtist :: APIKey -> SessionKey -> IO ()
addArtist ak sk = Library.addArtist (Artist "Mobthrow") ak sk >>= print ||| const (return ())

addTrack :: APIKey -> SessionKey -> IO ()
addTrack ak sk = Library.addTrack (Artist "Eminem") (Track "Kim") ak sk >>= print ||| const (return ())

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

removeAlbum :: APIKey -> SessionKey -> IO ()
removeAlbum ak sk = Library.removeAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") ak sk >>= print ||| const (return ())

removeArtist :: APIKey -> SessionKey -> IO ()
removeArtist ak sk = Library.removeArtist (Artist "Burzum") ak sk >>= print ||| const (return ())

removeTrack :: APIKey -> SessionKey -> IO ()
removeTrack ak sk = Library.removeTrack (Artist "Eminem") (Track "Kim") ak sk >>= print ||| const (return ())

removeScrobble :: APIKey -> SessionKey -> IO ()
removeScrobble ak sk = Library.removeScrobble (Artist "Gojira") (Track "Ocean") (Timestamp 1328905590) ak sk >>= print ||| const (return ())

common :: IO ()
common = do getAlbums
            getArtists
            getTracks

auth :: APIKey -> SessionKey -> IO ()
auth ak sk = do addAlbum ak sk
                addArtist ak sk
                addTrack ak sk
                removeAlbum ak sk
                removeArtist ak sk
                removeTrack ak sk
                removeScrobble ak sk
