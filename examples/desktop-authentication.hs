{-# LANGUAGE OverloadedStrings #-}
-- | Desktop application authentication flow example
--
-- Please remember to substitute __YOUR_API_KEY__
-- and __YOUR_SECRET__ for real values
module Main where

import Control.Lens
import Data.Aeson.Lens

import Network.Lastfm
import Network.Lastfm.Authentication


main :: IO ()
main = do
  r <- lastfm $ getToken <*> apiKey ak <* json
  whenJust (r ^. key "token") $ \t -> do
    putStrLn $ "approve: " ++ link (apiKey ak <* token t)
    _ <- getChar
    r' <- lastfm . sign s $ getSession <*> token t <*> apiKey ak <* json
    whenJust (r' ^. key "session" . key "key") $ \sk ->
      putStrLn $ "session key: " ++ sk
 where
  ak = "__YOUR_API_KEY__"
  s = "__YOUR_SECRET__"


whenJust :: Monad m => Maybe a -> (a -> m b) -> m ()
whenJust (Just x) f = f x >> return ()
whenJust _ _ = return ()
