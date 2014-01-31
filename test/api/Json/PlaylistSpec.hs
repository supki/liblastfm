{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.PlaylistSpec (spec) where

import Control.Lens
import Data.Aeson.Lens
import Data.Int (Int64)
import Data.Text.Lens (unpacked)
import Network.Lastfm hiding (to)
import Network.Lastfm.Playlist
import Network.Lastfm.User
import Test.Hspec
import Test.HUnit (assertFailure)
import Text.Printf
import Text.Read (readMaybe)

import SpecHelper


spec :: Spec
spec = do
  it "create" $ -- Order matters.
    privately (create <* title "Awesome playlist")
   `shouldHaveJson`
    key "playlists".key "playlist".key "title"._String

  it "addTrack" $ do
    r <- lastfm $ getPlaylists <*> user "liblastfm" <*> ak' <* json
    case r of
      Left e ->
        assertFailure (printf "last.fm error: %s" (show e))
      Right val ->
        case preview pl val of
          Just (Just pid) -> shouldHaveJson_ . privately $
            addTrack <*> playlist pid <*> artist "Ruby my dear" <*> track "Chazz"
          _ -> assertFailure (printf "bad JSON object: %s" (show val))

ak' :: Request f APIKey
ak' = apiKey "29effec263316a1f8a97f753caaa83e0"

pl :: Query JSON (Maybe Int64)
pl = key "playlists".key "playlist".values.key "id"._String.unpacked.to readMaybe
