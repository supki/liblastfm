{-# LANGUAGE OverloadedStrings #-}
module Network.Lastfm.JSON.Auth where

import Control.Applicative ((<$>), empty)
import Control.Monad (liftM)
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Lastfm

instance FromJSON Token where
  parseJSON (Object v) = Token <$> v .: "token"
  parseJSON _ = empty

instance FromJSON SessionKey where
  parseJSON (Object v) = SessionKey <$> ((v .: "session") >>= (.: "key"))
  parseJSON _ = empty

getToken :: Either LastfmError String -> Either LastfmError Token
getToken = liftM (fromJust . decode . BL.pack)

getSession :: Either LastfmError String -> Either LastfmError SessionKey
getSession = liftM (fromJust . decode . BL.pack)
