module ELibrary (start) where

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Library as Library

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user = User "smpcln"

addAlbum :: APIKey -> SessionKey -> IO ()
addAlbum apiKey sessionKey = do response <- Library.addAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") apiKey sessionKey
                                case response of
                                  Left e  -> print e
                                  Right () -> return ()

addArtist :: APIKey -> SessionKey -> IO ()
addArtist apiKey sessionKey = do response <- Library.addArtist (Artist "Mobthrow") apiKey sessionKey
                                 case response of
                                   Left e  -> print e
                                   Right () -> return ()

addTrack :: APIKey -> SessionKey -> IO ()
addTrack apiKey sessionKey = do response <- Library.addTrack (Artist "Eminem") (Track "Kim") apiKey sessionKey
                                case response of
                                  Left e  -> print e
                                  Right () -> return ()

getAlbums :: IO ()
getAlbums = do response <- Library.getAlbums user (Just $ Artist "Burzum") Nothing (Just $ Limit 5) apiKey
               putStr "Top 5 popular Burzum albums: "
               case response of
                 Left e  -> print e
                 Right r -> print $ albums r
               putStrLn ""
  where albums = mapM (getContent <=< lookupChild "name") <=< lookupChildren "album" <=< lookupChild "albums" <=< wrap

getArtists :: IO ()
getArtists = do response <- Library.getArtists user Nothing (Just $ Limit 7) apiKey
                putStr "Top 7 popular artists playcounts: "
                case response of
                  Left e  -> print e
                  Right r -> print $ playcounts r
                putStrLn ""
  where playcounts = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists" <=< wrap

getTracks :: IO ()
getTracks = do response <- Library.getTracks user (Just $ Artist "Burzum") Nothing Nothing (Just $ Limit 4) apiKey
               putStr "First 4 Burzum tracks: "
               case response of
                 Left e  -> print e
                 Right r -> print $ tracks r
               putStrLn ""
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "tracks" <=< wrap


removeAlbum :: APIKey -> SessionKey -> IO ()
removeAlbum apiKey sessionKey = do response <- Library.removeAlbum (Artist "Franz Ferdinand") (Album "Franz Ferdinand") apiKey sessionKey
                                   case response of
                                     Left e  -> print e
                                     Right () -> return ()

removeArtist :: APIKey -> SessionKey -> IO ()
removeArtist apiKey sessionKey = do response <- Library.removeArtist (Artist "Burzum") apiKey sessionKey
                                    case response of
                                      Left e  -> print e
                                      Right () -> return ()

removeTrack :: APIKey -> SessionKey -> IO ()
removeTrack apiKey sessionKey = do response <- Library.removeTrack (Artist "Eminem") (Track "Kim") apiKey sessionKey
                                   case response of
                                     Left e  -> print e
                                     Right () -> return ()

start :: IO ()
start = do getAlbums
           getArtists
           getTracks
           (apiKey, sessionKey, secret) <- getConfig ".lastfm.conf"
           withSecret secret $ do addAlbum apiKey sessionKey
                                  addArtist apiKey sessionKey
                                  addTrack apiKey sessionKey
                                  removeAlbum apiKey sessionKey
                                  removeArtist apiKey sessionKey
                                  removeTrack apiKey sessionKey
           -- removeScrobble (requires track.scrobble implemented)
