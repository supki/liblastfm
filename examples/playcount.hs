{-# LANGUAGE OverloadedStrings #-}
{- This example shows how to get user playcount with lens-aeson
 -
 - Sample session:
 -
 - $> ./examples/playcount.hs
 - >>> MCDOOMDESTROYER
 - 96698
 - >>> smpcln
 - 95518
 - >>> ^C
 - $>
 -}
import           Control.Lens                -- lens
import           Control.Lens.Aeson          -- lens-aeson
import           Control.Monad               -- base
import           Data.Maybe                  -- base
import           Data.Text (Text)            -- text
import qualified Data.Text.IO as Text        -- text
import           Network.Lastfm              -- liblastfm
import qualified Network.Lastfm.User as User -- liblastfm
import           System.IO (hFlush, stdout)  -- base


main :: IO ()
main = forever $ Text.putStr ">>> " >> hFlush stdout >> Text.getLine >>= playcount >>= Text.putStrLn

playcount :: Text -> IO Text
playcount u =
  lastfm (User.getInfo <*> user u <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json) <&> \rsp ->
    fromMaybe "Playcount not found" (rsp ^? folded.key "user".key "playcount"._String)
