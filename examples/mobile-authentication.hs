{-# LANGUAGE OverloadedStrings #-}
-- | Mobile application authentication flow example
--
-- Please remember to substitute __YOUR_API_KEY__,
-- __YOUR_SECRET__, __USERNAME__ and __PASSWORD__
-- for real values
import           Control.Lens                  -- lens
import           Data.Aeson.Lens               -- lens-aeson
import qualified Data.Text as Text             -- text
import qualified Data.Text.IO as Text          -- text
import           Network.Lastfm                -- liblastfm
import           Network.Lastfm.Authentication -- liblastfm

main :: IO ()
main = do
  r <- lastfm . sign s $ getMobileSession <*> username u <*> password p <*> apiKey ak <* json
  let maybeSk = r ^? folded.key "session".key "key"._String
  Text.putStrLn $ case maybeSk of
    Just sk -> "Mobile session key: " `Text.append` sk
    Nothing -> "Mobile session key wasn't retrieved, something goes wrong"
 where
  ak = "__YOUR_API_KEY__"
  s  = "__YOUR_SECRET__"
  u  = "__USERNAME__"
  p  = "__PASSWORD__"
