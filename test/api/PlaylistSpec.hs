{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module PlaylistSpec (spec) where

import Control.Lens
import Control.Lens.Aeson
import Data.Int (Int64)
import Data.Text (Text)
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
  it "Playlist.create" $ -- Order matters.
    query pn . sign privateSecret $
      create <* title "Awesome playlist"
      <*> privateAPIKey <*> privateSessionKey

  it "Playlist.addTrack" $ do
    r <- lastfm $ getPlaylists <*> user "liblastfm" <*> ak' <* json
    case r of
      Left e ->
        assertFailure (printf "last.fm error: %s" (show e))
      Right val ->
        case preview pl val of
          Just (Just pid) -> query_ . sign privateSecret $
            addTrack <*> playlist pid <*> artist "Ruby my dear" <*> track "Chazz"
            <*> privateAPIKey <*> privateSessionKey
          _ -> assertFailure (printf "bad JSON object: %s" (show val))

ak' :: Request f APIKey
ak' = apiKey "29effec263316a1f8a97f753caaa83e0"

pl :: Query (Maybe Int64)
pl = key "playlists".key "playlist".values.key "id"._String.unpacked.to readMaybe

pn :: Query Text
pn = key "playlists".key "playlist".key "title"._String
