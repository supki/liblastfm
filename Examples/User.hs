import Network.Lastfm.Core
import Network.Lastfm.Types (APIKey(..), Limit(..), User(..))
import Network.Lastfm.API.User

import Data.List.Split (splitEvery)

getPlayCount :: User -> APIKey -> IO ()
getPlayCount user apiKey = do
  response <- getInfo (Just user) apiKey
  case response of
    Left e -> print e
    Right r -> printPlayCount . firstInnerTagContent "playcount" $ r
      where
        printPlayCount Nothing = putStrLn ""
        printPlayCount (Just a) = putStrLn a

getAllLovedTracks :: User -> Limit -> APIKey -> IO ()
getAllLovedTracks user limit apiKey = do
  response <- getLovedTracks user (Just limit) Nothing apiKey
  case response of 
    Left e -> print e
    Right r -> mapM_ (putStrLn . (\[a,b] -> b ++ " - " ++ a )) . splitEvery 2 . allInnerTagsContent "name" $ r

main = do
  let apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
  let user = User "smpcln"
  putStrLn "Play count:"
  getPlayCount user apiKey
  putStrLn "Last 10 loved tracks:"
  getAllLovedTracks user (Limit 10) apiKey
