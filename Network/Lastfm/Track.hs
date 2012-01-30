module Network.Lastfm.Track
  ( love, unlove
  ) where

import Network.Lastfm.Core

type APIKey = String
type Artist = String
type Track = String
type SessionKey = String

love :: Track -> Artist -> APIKey -> SessionKey -> IO ()
love track artist apiKey sessionKey = callAPI
  [ ("method","track.love")
  , ("track", track)
  , ("artist", artist)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ] >> return ()

unlove :: Track -> Artist -> APIKey -> SessionKey -> IO ()
unlove track artist apiKey sessionKey = callAPI
  [ ("method","track.unlove")
  , ("track", track)
  , ("artist", artist)
  , ("api_key", apiKey)
  , ("sk", sessionKey)
  ] >> return ()
