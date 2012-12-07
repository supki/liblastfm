{-# LANGUAGE OverloadedStrings #-}
-- | Web application authentication flow example
module Main where

import Control.Monad
import Data.IORef

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens
import Happstack.Server hiding (host, path)

import Network.Lastfm
import Network.Lastfm.Authentication


main :: IO ()
main = do
  sessions <- newIORef []
  simpleHTTP nullConf $ msum
    [ dir "authenticate" $ seeOther (link $ apiKey ak <> callback __YOUR_CALLBACK__) ""
    , dir "save" $ do
        t <- lookText "token"
        r <- liftIO . lastfm . sign s $ getSession t <> apiKey ak <> json
        case r ^. key "session" . key "key" . asText of
          Just sk -> liftIO $ modifyIORef' sessions (sk:)
          Nothing -> return ()
        ok "Saved."
    , dir "show" $ liftIO (readIORef sessions) >>= ok . show
    ]
 where
  ak = __YOUR_API_KEY__
  s = __YOUR_SECRET__
