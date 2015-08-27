{-# LANGUAGE OverloadedStrings #-}
-- This example shows how to get user playcount with lens-aeson
--
-- Sample session:
--
-- $> ./examples/playcount.hs
-- >>> MCDOOMDESTROYER
-- 96698
-- >>> smpcln
-- 95518
-- >>> ^C
-- $>
--}
import           Control.Lens          -- lens
import           Data.Aeson.Lens       -- lens-aeson
import           Control.Monad         -- base
import           Data.Text (Text)      -- text
import qualified Data.Text.IO as Text  -- text
import           Lastfm                -- liblastfm
import qualified Lastfm.User as User   -- liblastfm
import qualified System.IO as IO       -- base


main :: IO ()
main = withConnection $ \conn ->
  forever $ do Text.putStr ">>> "; flush; Text.getLine >>= playcount conn >>= Text.putStrLn

flush :: IO ()
flush = IO.hFlush IO.stdout

playcount :: Connection -> Text -> IO Text
playcount conn u = do
  res <- lastfm conn
    (User.getInfo <*> user u <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json)
  return $ case res ^? folded.key "user".key "playcount"._String of
    Nothing -> "Playcount not found"
    Just x  -> x
{-# ANN playcount ("HLint: ignore Use fromMaybe" :: String) #-}
