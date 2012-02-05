#!/usr/bin/env runhaskell

import Control.Monad ((<=<))
import Prelude hiding (compare)

import Network.Lastfm.API.Tasteometer
import Network.Lastfm.Core
import Network.Lastfm.Types

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
user1 = ValueUser . User $ "smpcln"
user2 = ValueUser . User $ "ingolfr"

-- compare example
getScore :: IO ()
getScore = do response <- compare user1 user2 (Just . Limit $ 10) apiKey
              putStr "Score: "
              case response of
                Left e -> print e
                Right r -> print $ score r
  where score = getContent <=< lookupChild "score" <=< lookupChild "result" <=< lookupChild "comparison"

-- compare example
getSimilarArtists :: IO ()
getSimilarArtists = do response <- compare user1 user2 (Just . Limit $ 10) apiKey
                       putStr "Similar groups: "
                       case response of
                         Left e -> print e
                         Right r -> print $ similarArtists r
  where similarArtists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists" <=< lookupChild "result" <=< lookupChild "comparison"

main = do
  getScore
  getSimilarArtists
