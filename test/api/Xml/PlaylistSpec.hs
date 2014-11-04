{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.PlaylistSpec (spec) where

import Control.Lens
import Data.Int (Int64)
import Data.Text.Lens (unpacked)
import Network.Lastfm hiding (to)
import Network.Lastfm.Playlist
import Network.Lastfm.User
import Test.Hspec
import Test.HUnit (assertFailure)
import Text.Printf
import Text.Read (readMaybe)
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "create" $ -- Order matters.
    privately (create <* title "Awesome playlist")
   `shouldHaveXml`
    root.node "playlists".node "playlist".node "title".text

  it "addTrack" $ do
    r <- lastfm man $ getPlaylists <*> user "liblastfm" <*> ak' <* Network.Lastfm.xml
    case r of
      Left e ->
        assertFailure (printf "last.fm error: %s" (show e))
      Right val ->
        case preview pl val of
          Just (Just pid) -> shouldHaveXml_ . privately $
            addTrack <*> playlist pid <*> artist "Ruby my dear" <*> track "Chazz"
          _ -> assertFailure (printf "bad XML object: %s" (show val))

ak' :: Request f APIKey
ak' = apiKey "29effec263316a1f8a97f753caaa83e0"

pl :: Fold Document (Maybe Int64)
pl = root.node "playlists".node "playlist".node "id".text.unpacked.to readMaybe
