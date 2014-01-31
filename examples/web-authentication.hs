{-# LANGUAGE OverloadedStrings #-}
-- | Web application authentication flow example
--
-- Please remember to substitute __YOUR_API_KEY__,
-- __YOUR_SECRET__ and __YOUR_CALLBACK__ for real values
import Control.Lens                    -- lens
import Data.Aeson.Lens                 -- lens
import Control.Monad                   -- base
import Control.Monad.IO.Class (liftIO) -- transformers
import Data.IORef                      -- base
import Happstack.Server                -- happstack-server
import Network.Lastfm                  -- liblastfm
import Network.Lastfm.Authentication   -- liblastfm


main :: IO ()
main = do
  sessions <- newIORef []
  simpleHTTP nullConf $ msum
    [ dir "authenticate" $ seeOther (link $ apiKey ak <* callback "__YOUR_CALLBACK__") ""
    , dir "save" $ do
        t <- lookText' "token"
        r <- liftIO . lastfm . sign s $ getSession <*> token t <*> apiKey ak <* json
        case r ^? folded.key "session".key "key"._String of
          Just sk -> liftIO $ modifyIORef' sessions (sk:)
          Nothing -> return ()
        ok "Saved."
    , dir "show" $ liftIO (readIORef sessions) >>= ok . show
    ]
 where
  ak = "__YOUR_API_KEY__"
  s  = "__YOUR_SECRET__"
