module Network.Lastfm.Auth
  ( getToken
  , getAuthorizeTokenLink
  , getSession
  ) where

import Control.Applicative ((<$>))
import Network.Lastfm.Core

type APIKey = String
type Token = String
type SessionKey = String

getSession :: APIKey -> Token -> IO (Maybe SessionKey)
getSession apiKey token = tagContent "key" <$> callAPI [ ("method","auth.getSession")
                                                       , ("api_key", apiKey)
                                                       , ("token", token)
                                                       ]

getToken :: APIKey -> IO (Maybe Token)
getToken apiKey = tagContent "token" <$> callAPI [ ("method","auth.getToken")
                                                 , ("api_key", apiKey)
                                                 ]


getAuthorizeTokenLink :: APIKey -> Token -> String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ apiKey ++ "&token=" ++ token
