{-# LANGUAGE OverloadedStrings #-}
-- | Desktop application authentication flow example
--
-- Please remember to substitute __YOUR_API_KEY__
-- and __YOUR_SECRET__ for real values
import Control.Lens                  -- lens
import Data.Aeson.Lens               -- lens-aeson
import Data.Foldable (for_)          -- base
import Data.Text (unpack)            -- text
import Network.Lastfm                -- liblastfm
import Network.Lastfm.Authentication -- liblastfm


main :: IO ()
main = withConnection $ \conn -> do
  r <- lastfm conn $ getToken <*> apiKey ak <* json
  for_ (r ^? folded.key "token"._String) $ \t -> do
    putStrLn $ "approve: " ++ link (apiKey ak <* token t)
    _ <- getChar
    r' <- lastfm conn . sign s $ getSession <*> token t <*> apiKey ak <* json
    for_ (r' ^? folded.key "session".key "key"._String) $ \sk ->
      putStrLn $ "session key: " ++ unpack sk
 where
  ak = "__YOUR_API_KEY__"
  s  = "__YOUR_SECRET__"
