{-# LANGUAGE OverloadedStrings #-}
-- | Desktop application authentication flow example
--
-- Please remember to substitute __YOUR_API_KEY__,
-- __YOUR_SECRET__, __USERNAME__ and __PASSWORD__
-- for real values
module Main where

import Control.Lens
import Control.Lens.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.Lastfm
import Network.Lastfm.Authentication

main :: IO ()
main = do
  r <- lastfm . sign s $ getMobileSession <*> username u <*> password p <*> apiKey ak <* json
  let maybeSk = r ^? _Just . key "session" . key "key" . _String
  T.putStrLn $ case maybeSk of
    Just sk -> "Mobile session key: " `T.append` sk
    Nothing -> "Mobile session key wasn't retrieved, something goes wrong"
 where
  ak = "__YOUR_API_KEY__"
  s = "__YOUR_SECRET__"
  u = "__USERNAME__"
  p = "__PASSWORD__"
