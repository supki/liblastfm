module ELibrary (start) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.List.Split (splitOn)

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Library as Library

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user = User "smpcln"

addArtist :: APIKey -> SessionKey -> IO ()
addArtist apiKey sessionKey = do response <- Library.addArtist (Artist "Mobthrow") apiKey sessionKey
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

getConfig :: FilePath -> IO (APIKey, SessionKey, String)
getConfig fp = do [apiKey, sessionKey, secret] <- map ((!! 1) . splitOn "=" . filter (not . isSpace)) . lines <$> readFile fp
                  return (APIKey apiKey, SessionKey sessionKey, secret)

removeArtist :: APIKey -> SessionKey -> IO ()
removeArtist apiKey sessionKey = do response <- Library.removeArtist (Artist "Burzum") apiKey sessionKey
                                    case response of
                                      Left e  -> print e
                                      Right () -> return ()

start :: IO ()
start = do (apiKey, sessionKey, secret) <- getConfig ".lastfm.conf"
           -- addAlbum (requires authorization)
           withSecret secret $ addArtist apiKey sessionKey
           -- addTrack (requires authorization)
           getAlbums
           getArtists
           getTracks
           -- removeAlbum (requires authorization)
           withSecret secret $ removeArtist apiKey sessionKey
           -- removeScrobble (requires track.scrobble implemented)
           -- removeTrack (requires authorization)
