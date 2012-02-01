import Network.Lastfm.Auth (APIKey(..), SessionKey)
import Network.Lastfm.Core
import Network.Lastfm.User

import Control.Applicative ((<$>))
import Data.Maybe

getPlayCount :: User -> APIKey -> IO (Maybe String)
getPlayCount user apiKey =
  firstInnerTagContent "playcount" <$> getInfo (Just user) apiKey

main = do
  getPlayCount (User "smpcln") (APIKey "b25b959554ed76058ac220b7b2e0a026") >>= putStrLn . fromJust
