module Network.Lastfm.API.Auth
  ( getMobileSession, getSession, getToken
  , getAuthorizeTokenLink
  ) where

import Control.Applicative ((<$>))
import Control.Monad ((<=<), liftM)

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), APIKey, AuthToken, SessionKey(..), Token(..), User, unpack)

getMobileSession :: User -> APIKey -> AuthToken -> Lastfm (Maybe SessionKey)
getMobileSession user apiKey token = dispatch $ liftM (SessionKey . getContent) <$> lookupChild "key" <$> callAPI "auth.getMobileSession"
  [ "username" ?< user
  , "authToken" ?< token
  , "api_key" ?< apiKey
  ]

getSession :: APIKey -> Token -> Lastfm (Maybe SessionKey)
getSession apiKey token = dispatch $ liftM (SessionKey . getContent) <$> (lookupChild "key" <=< lookupChild "session") <$> callAPI "auth.getSession" ["api_key" ?< apiKey, "token" ?< token]

getToken :: APIKey -> Lastfm (Maybe Token)
getToken apiKey = dispatch $ liftM (Token . getContent) <$> lookupChild "token" <$> callAPI "auth.getToken" ["api_key" ?< apiKey]

getAuthorizeTokenLink :: APIKey -> Token -> String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ unpack apiKey ++ "&token=" ++ unpack token
