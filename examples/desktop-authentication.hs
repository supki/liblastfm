{-# LANGUAGE OverloadedStrings #-}
-- | Desktop application authentication flow example
--
-- Please remember to substitute __YOUR_API_KEY__
-- and __YOUR_SECRET__ for real values
module Main where

import Data.Foldable (for_)

import Control.Lens
import Control.Lens.Aeson
import Data.Text (unpack)

import Network.Lastfm
import Network.Lastfm.Authentication


main :: IO ()
main = do
  r <- lastfm $ getToken <*> apiKey ak <* json
  for_ (r ^? _Just . key "token" . _String) $ \t -> do
    putStrLn $ "approve: " ++ link (apiKey ak <* token t)
    _ <- getChar
    r' <- lastfm . sign s $ getSession <*> token t <*> apiKey ak <* json
    for_ (r' ^? _Just . key "session" . key "key" . _String) $ \sk ->
      putStrLn $ "session key: " ++ unpack sk
 where
  ak = "__YOUR_API_KEY__"
  s = "__YOUR_SECRET__"
