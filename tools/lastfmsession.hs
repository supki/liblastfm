{- Another example of using liblastfm.
 - It simplifies process of getting Session Key.
 -}

import Control.Applicative ((<$>))
import Control.Arrow ((|||))
import Control.Monad (liftM)
import Network.Lastfm
import Network.Lastfm.API.Auth
import System.Environment (getArgs)

main :: IO ()
main = do
  [ak, secret] <- parseArgs <$> getArgs
  token <- liftM parseToken $ getToken (APIKey ak)
  putStrLn $ "Authorize your token: " ++ getAuthorizeTokenLink (APIKey ak) (Token token)
  _ <- getLine
  sessionKey <- liftM parseSessionKey $ getSession (APIKey ak) (Token token) (Secret secret)
  putStrLn "APIKey, SessionKey, Secret:"
  mapM_ putStrLn [ak, sessionKey, secret]
    where
      parseArgs xs | length xs /= 2 = error "Usage: runhaskell lastfmsession.hs apiKey secret"
                   | otherwise      = xs
      parseSessionKey = ((error . show) ||| (\(SessionKey t) -> t))
      parseToken      = ((error . show) ||| (\(Token t) -> t))
