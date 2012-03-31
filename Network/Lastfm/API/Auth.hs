-- | Auth API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Auth
  ( getMobileSession, getSession, getToken
  , getAuthorizeTokenLink
  ) where

import Control.Monad.Error (runErrorT, throwError)
import Network.Lastfm

-- | Create a web service session for a user. Used for authenticating a user when the password can be inputted by the user. Only suitable for standalone mobile devices.
--
-- More: <http://www.lastfm.ru/api/show/auth.getMobileSession>
getMobileSession :: Username -> APIKey -> AuthToken -> Lastfm Response
getMobileSession username apiKey token = runErrorT . callAPI $
  [ (#) (Method "auth.getMobileSession")
  , (#) username
  , (#) token
  , (#) apiKey
  ]

-- | Fetch a session key for a user.
--
-- More: <http://www.lastfm.ru/api/show/auth.getSession>
getSession :: APIKey -> Token -> Lastfm Response
getSession apiKey token = runErrorT . callAPI $
  [ (#) (Method "auth.getSession")
  , (#) apiKey
  , (#) token
  ]

-- | Fetch an unathorized request token for an API account.
--
-- More: <http://www.lastfm.ru/api/show/auth.getToken>
getToken :: APIKey -> Lastfm Response
getToken apiKey = runErrorT . callAPI $
  [ (#) (Method "auth.getToken")
  , (#) apiKey
  ]

-- | Construct the link to authorize token.
getAuthorizeTokenLink :: APIKey -> Token -> String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ value apiKey ++ "&token=" ++ value token
