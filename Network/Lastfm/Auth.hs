module Network.Lastfm.Auth
  ( getMobileSession
  , getSession
  , getToken
  ) where

import Control.Monad (liftM)
import Network.Lastfm.Core

type APIKey = String
type Token = String

getMobileSession = undefined

getSession = undefined

getToken :: APIKey -> IO (Maybe Token)
getToken apiKey = liftM (tagContent "token") (callAPI [ ("method","auth.gettoken"), ("api_key", apiKey) ])
