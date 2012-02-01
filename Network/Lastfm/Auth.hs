{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Auth
  ( APIKey(..), Token(..), SessionKey(..)
  , getToken, getAuthorizeTokenLink, getSession
  ) where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Network.Lastfm.Core

newtype APIKey = APIKey String deriving (Show, LastfmValue)
newtype Token = Token String deriving (Show, LastfmValue)
newtype SessionKey = SessionKey String deriving (Show, LastfmValue)

getSession :: APIKey -> Token -> Lastfm (Maybe SessionKey)
getSession apiKey token = liftM SessionKey <$> firstInnerTagContent "key" <$> callAPI "auth.getSession" ["api_key" ?< apiKey, "token" ?< token]

getToken :: APIKey -> Lastfm (Maybe Token)
getToken apiKey = liftM Token <$> firstInnerTagContent "token" <$> callAPI "auth.getToken" ["api_key" ?< apiKey]

getAuthorizeTokenLink :: APIKey -> Token -> String
getAuthorizeTokenLink apiKey token = "http://www.last.fm/api/auth/?api_key=" ++ unpack apiKey ++ "&token=" ++ unpack token
