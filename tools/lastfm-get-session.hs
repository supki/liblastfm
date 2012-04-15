#!/usr/bin/env runhaskell
{- Another example of using liblastfm.
 - It simplifies Session Key retrieval.
 -}

import Control.Applicative ((<$>))
import Control.Arrow ((|||))
import Network.Lastfm
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Network.Lastfm.API.Auth as Auth

confFile = ".lastfm.conf"

parseArgs xs
  | length xs /= 2 = error "Usage: ./lastfm-get-session.hs apiKey secret"
  | otherwise = xs

main :: IO ()
main = do
  [ak, s] <- parseArgs <$> getArgs
  let apiKey = APIKey ak
      secret = Secret s
  confFilePath <- (</> confFile) <$> getHomeDirectory
  token <- getToken <$> Auth.getToken apiKey
  putStrLn $ "Authorize your token: " ++ Auth.getAuthorizeTokenLink apiKey token
  _ <- getChar
  sk <- getSession <$> Auth.getSession apiKey token secret
  writeFile confFilePath $ printf "APIKey = %s\nSessionKey = %s\nSecret = %s\n" ak sk s
  putStrLn $ "Session key is successfuly written to " ++ confFilePath
    where
      getToken = (error . show) ||| id
      getSession = (error . show ||| (\(SessionKey t) -> t))
