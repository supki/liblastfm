module Network.Lastfm.API.Auth
  ( getMobileSession, getSession, getToken
  , getAuthorizeTokenLink
  ) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<), liftM)

import Network.Lastfm.Response
import Network.Lastfm.Types ((?<), APIKey, AuthToken, SessionKey(..), Token(..), User, unpack)

getMobileSession :: User -> APIKey -> AuthToken -> Lastfm Response
getMobileSession user apiKey token = dispatch $ callAPI "auth.getMobileSession"
  [ "username" ?< user
  , "authToken" ?< token
  , "api_key" ?< apiKey
  ]

getSession :: APIKey -> Token -> Lastfm Response
getSession apiKey token = dispatch $ callAPI "auth.getSession" ["api_key" ?< apiKey, "token" ?< token]

getToken :: APIKey -> Lastfm Response
getToken apiKey = dispatch $ callAPI "auth.getToken" ["api_key" ?< apiKey]

getAuthorizeTokenLink :: APIKey -> Token -> String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ unpack apiKey ++ "&token=" ++ unpack token
