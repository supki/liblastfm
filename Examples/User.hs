import Network.Lastfm.Auth (APIKey(..), SessionKey)
import Network.Lastfm.Core
import Network.Lastfm.User

import Control.Applicative ((<$>))
import Data.List.Split (splitEvery)
import Data.Maybe (fromJust)

getPlayCount :: User -> APIKey -> IO (Maybe String)
getPlayCount user apiKey =
  firstInnerTagContent "playcount" <$> getInfo (Just user) apiKey

getAllLovedTracks :: User -> Limit -> APIKey -> IO [String]
getAllLovedTracks user limit apiKey
  = map (\[a,b] -> b ++ " - " ++ a ) . splitEvery 2 . allInnerTagsContent "name"
  <$> getLovedTracks user (Just limit) Nothing apiKey

main = do
  let apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
  let user = User "smpcln"
  putStrLn "Play count:"
  getPlayCount user apiKey >>= putStrLn . fromJust
  putStrLn "Last 10 loved tracks:"
  getAllLovedTracks user (Limit 10) apiKey >>= mapM_ putStrLn
