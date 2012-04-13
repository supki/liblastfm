{- Another example of using liblastfm.
 - It simplifies process of getting Session Key.
 -}

import Control.Applicative ((<$>))
import Control.Arrow ((|||))
import Network.Lastfm
import Network.Lastfm.API.Auth
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

confFile = ".lastfm.conf"

main :: IO ()
main = do
  [ak, secret] <- parseArgs <$> getArgs
  confFilePath <- flip (</>) confFile <$> getHomeDirectory
  token <- parseToken <$> getToken (APIKey ak)
  putStrLn $ "Authorize your token: " ++ getAuthorizeTokenLink (APIKey ak) (Token token)
  _ <- getLine
  sessionKey <- parseSessionKey <$> getSession (APIKey ak) (Token token) (Secret secret)
  writeFile confFilePath $ printf "APIKey = %s\nSessionKey = %s\nSecret = %s\n" ak sessionKey secret
  putStrLn $ "Session key is successfuly written to " ++ confFilePath
    where
      parseArgs xs | length xs /= 2 = error "Usage: runhaskell lastfmsession.hs apiKey secret"
                   | otherwise      = xs
      parseSessionKey = ((error . show) ||| (\(SessionKey t) -> t))
      parseToken = ((error . show) ||| (\(Token t) -> t))
