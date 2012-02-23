-- | Auth API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Auth
  ( getMobileSession, getSession, getToken
  , getAuthorizeTokenLink
  ) where

import Network.Lastfm ( Lastfm, Response, callAPI, dispatch
                      , (?<), APIKey, AuthToken, Token(..), User, unpack
                      )

-- | Create a web service session for a user. Used for authenticating a user when the password can be inputted by the user. Only suitable for standalone mobile devices.
--
-- More: <http://www.lastfm.ru/api/show/auth.getMobileSession>
getMobileSession :: User -> APIKey -> AuthToken -> Lastfm Response
getMobileSession user apiKey token = dispatch . callAPI "auth.getMobileSession" $
  [ "username" ?< user
  , "authToken" ?< token
  , "api_key" ?< apiKey
  ]

-- | Fetch a session key for a user.
--
-- More: <http://www.lastfm.ru/api/show/auth.getSession>
getSession :: APIKey -> Token -> Lastfm Response
getSession apiKey token = dispatch . callAPI "auth.getSession" $ ["api_key" ?< apiKey, "token" ?< token]

-- | Fetch an unathorized request token for an API account.
--
-- More: <http://www.lastfm.ru/api/show/auth.getToken>
getToken :: APIKey -> Lastfm Response
getToken apiKey = dispatch . callAPI "auth.getToken" $ ["api_key" ?< apiKey]

-- | Construct the link to authorize token.
getAuthorizeTokenLink :: APIKey -> Token -> String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ unpack apiKey ++ "&token=" ++ unpack token
