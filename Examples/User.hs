import Network.Lastfm.Core (allInnerTagsContent, firstInnerTagContent)
import Network.Lastfm.Types
import Network.Lastfm.API.User

import Data.List (intercalate)
import Data.List.Split (splitEvery)

-- getArtistTracks
getArtistTracksExample :: Int -> User -> Artist -> APIKey -> IO ()
getArtistTracksExample n user artist apiKey = do
  response <- getArtistTracks user artist Nothing Nothing Nothing apiKey
  case response of
    Left e -> print e
    Right r -> mapM_ putStrLn . take n . allInnerTagsContent "name" $ r

-- getBannedTracks
getBannedArtists :: User -> Limit -> APIKey -> IO ()
getBannedArtists user limit apiKey = do
  response <- getBannedTracks user (Just limit) Nothing apiKey
  case response of
    Left e -> print e
    Right r -> mapM_ putStrLn . allInnerTagsContent "name" $ r

-- getEvents
getEventsExample :: User -> Limit -> APIKey -> IO ()
getEventsExample user limit apiKey = do
  response <- getEvents user Nothing (Just limit) Nothing apiKey
  case response of
    Left e -> print e
    Right r -> mapM_ putStrLn . map (\(title,url) -> title ++ " (" ++ url ++ ")") $ zip (allInnerTagsContent "title" r) (allInnerTagsContent "url" r)

-- getFriends
getFriendsList :: User -> Limit -> APIKey -> IO ()
getFriendsList user limit apiKey = do
  response <- getFriends user Nothing (Just limit) Nothing apiKey
  case response of
    Left e -> print e
    Right r -> mapM_ putStrLn . allInnerTagsContent "name" $ r

-- getInfo
getPlayCount :: User -> APIKey -> IO ()
getPlayCount user apiKey = do
  response <- getInfo (Just user) apiKey
  case response of
    Left e -> print e
    Right r -> printPlayCount . firstInnerTagContent "playcount" $ r
      where
        printPlayCount Nothing = putStrLn ""
        printPlayCount (Just a) = putStrLn a

-- getLovedTracks
getAllLovedTracks :: User -> Limit -> APIKey -> IO ()
getAllLovedTracks user limit apiKey = do
  response <- getLovedTracks user (Just limit) Nothing apiKey
  case response of
    Left e -> print e
    Right r -> mapM_ (putStrLn . intercalate " - ") . splitEvery 2 . allInnerTagsContent "name" $ r

main = do
  let apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
  let user1 = User "smpcln"
  let user2 = User "mokele"
  let artist = Artist "Dvar"
  putStrLn "\nPlay count:"; getPlayCount user1 apiKey
  putStrLn "\nLast 10 loved tracks:"; getAllLovedTracks user1 (Limit 10) apiKey
  putStrLn $ "\nSome " ++ show artist ++ "s tracks:"; getArtistTracksExample 10 user1 artist apiKey
  putStrLn "\nBanned artists:"; getBannedArtists user1 (Limit 10) apiKey
  putStrLn $ "\n" ++ show user2 ++ "'s events:"; getEventsExample user2 (Limit 5) apiKey
  putStrLn $ "\n" ++ show user1 ++ "'s friends:"; getFriendsList user1 (Limit 5) apiKey
