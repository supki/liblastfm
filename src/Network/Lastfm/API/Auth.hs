module Network.Lastfm.API.Auth
  ( getMobileSession, getSession, getToken
  , getAuthorizeTokenLink
  ) where

import Data.Functor ((<$>))

import Network.Lastfm.Internal
import Network.Lastfm.JSON (json)


getMobileSession ∷ Username → APIKey → AuthToken → Lastfm SessionKey
getMobileSession username apiKey token = simple <$> callAPI json [(#) (Method "auth.getMobileSession"), (#) username, (#) token, (#) apiKey]


getSession ∷ APIKey → Token → Secret → Lastfm SessionKey
getSession apiKey token secret = simple <$> callAPIsigned json secret [(#) (Method "auth.getSession"), (#) apiKey, (#) token]


getToken ∷ APIKey → Lastfm Token
getToken apiKey = simple <$> callAPI json [(#) (Method "auth.getToken"), (#) apiKey]


getAuthorizeTokenLink ∷ APIKey → Token → String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ value apiKey ++ "&token=" ++ value token
