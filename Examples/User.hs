#!/usr/bin/env runhaskell

import Control.Monad ((<=<))
-- import Data.List (intercalate)
-- import Data.List.Split (splitEvery)

import Network.Lastfm.Core
import Network.Lastfm.Types
import Network.Lastfm.API.User

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user1 = User "smpcln"
user2 = User "mokele"
artist = Artist "Dvar"

-- getArtistTracks example
getArtistTracksExample :: IO ()
getArtistTracksExample = do response <- getArtistTracks user1 artist Nothing Nothing Nothing apiKey
                            putStr "Artist tracks: "
                            case response of
                              Left e -> print e
                              Right r -> print $ artistTracks r
  where artistTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "artisttracks"

-- getBannedTracks example
getBannedArtists :: IO ()
getBannedArtists = do response <- getBannedTracks user1 Nothing (Just $ Limit 10) apiKey
                      putStr "Banned artists: "
                      case response of
                        Left e -> print e
                        Right r -> print $ bannedArtists r
  where bannedArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "bannedtracks"

-- getEvents example
getEventsExample :: IO ()
getEventsExample = do response <- getEvents user2 Nothing (Just $ Limit 5) Nothing apiKey
                      putStr "Events: "
                      case response of
                        Left e -> print e
                        Right r -> print $ events r
  where events = mapM (getContent <=< lookupChild "url" <=< lookupChild "venue") <=< lookupChildren "event" <=< lookupChild "events"

-- getFriends example
getFriendsExample :: IO ()
getFriendsExample = do response <- getFriends user1 Nothing Nothing (Just $ Limit 10) apiKey
                       putStr "Friends: "
                       case response of
                         Left e -> print e
                         Right r -> print $ friends r 
  where friends = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "friends"

-- getInfo example
getPlayCount :: IO ()
getPlayCount = do response <- getInfo (Just user1) apiKey
                  putStr "Play count: "
                  case response of
                    Left e -> print e
                    Right r -> print $ playCount r
  where playCount = getContent <=< lookupChild "playcount" <=< lookupChild "user"

-- getLovedTracks example
getLovedTracksExample :: IO ()
getLovedTracksExample = do response <- getLovedTracks user1 Nothing (Just $ Limit 10) apiKey
                           putStr "Loved tracks: "
                           case response of
                             Left e -> print e
                             Right r -> print $ lovedTracks r
  where lovedTracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "lovedtracks"

main = do
  getArtistTracksExample
  getBannedArtists
  getEventsExample
  getFriendsExample
  getPlayCount
  getLovedTracksExample
