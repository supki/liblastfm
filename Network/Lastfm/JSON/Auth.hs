{-# LANGUAGE OverloadedStrings #-}
module Network.Lastfm.JSON.Auth where

import Control.Applicative ((<$>), empty)
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Lastfm

instance FromJSON Token where
  parseJSON (Object v) = Token <$> v .: "token"
  parseJSON _ = empty

getToken :: String -> Token
getToken = fromJust . decode . BL.pack
