#!/usr/bin/runhaskell
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
import           Control.Monad
import           Data.Maybe
import qualified System.IO as IO

import           Control.Lens
import           Control.Lens.Aeson
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Network.Lastfm
import qualified Network.Lastfm.User as U


main :: IO ()
main = forever $ T.putStr ">>> " >> IO.hFlush IO.stdout >> T.getLine >>= playcount >>= T.putStrLn


playcount :: Text -> IO Text
playcount u = lastfm (U.getInfo <*> user u <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json) <&> \rsp ->
  fromMaybe "Playcount not found" (rsp ^? _Just . key "user" . key "playcount" . _String)
