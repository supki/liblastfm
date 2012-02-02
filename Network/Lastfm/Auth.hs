module Network.Lastfm.Auth
  ( getMobileSession, getSession, getToken
  , getAuthorizeTokenLink
  ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), APIKey, AuthToken, SessionKey(..), Token(..), User, unpack)

getMobileSession :: User -> APIKey -> AuthToken -> Lastfm (Maybe SessionKey)
getMobileSession user apiKey token = dispatch $ liftM SessionKey <$> firstInnerTagContent "key" <$> callAPI "auth.getMobileSession"
  [ "username" ?< user
  , "authToken" ?< token
  , "api_key" ?< apiKey
  ]

getSession :: APIKey -> Token -> Lastfm (Maybe SessionKey)
getSession apiKey token = dispatch $ liftM SessionKey <$> firstInnerTagContent "key" <$> callAPI "auth.getSession" ["api_key" ?< apiKey, "token" ?< token]

getToken :: APIKey -> Lastfm (Maybe Token)
getToken apiKey = dispatch $ liftM Token <$> firstInnerTagContent "token" <$> callAPI "auth.getToken" ["api_key" ?< apiKey]

getAuthorizeTokenLink :: APIKey -> Token -> String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ unpack apiKey ++ "&token=" ++ unpack token
