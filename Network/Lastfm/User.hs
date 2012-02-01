{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.User
  ( User(..)
  , getInfo
  )where

import Network.Lastfm.Auth (APIKey, SessionKey)
import Network.Lastfm.Core

newtype User = User String deriving (Show, LastfmValue)

getInfo :: Maybe User -> APIKey -> IO Response
getInfo user apiKey = callAPI "user.getinfo" $
  [ "user" ?< user
  , "api_key" ?< apiKey
  ]
