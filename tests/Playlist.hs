{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Playlist (auth) where

import Control.Lens
import Control.Lens.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Lens (unpacked)
import Network.Lastfm hiding (to)
import Network.Lastfm.Playlist
import Network.Lastfm.User
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure)
import Text.Printf
import Text.Read (readMaybe)

import Helper


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Playlist.create"   testCreate -- Order matters.
  , testCase "Playlist.addTrack" testAddTrack
  ]
 where
  ak' = "29effec263316a1f8a97f753caaa83e0"

  testAddTrack = do
    r <- lastfm $ getPlaylists <*> user "liblastfm" <*> apiKey ak' <* json
    case r of
      Left e ->
        assertFailure (printf "last.fm error: %s" (show e))
      Right val ->
        case preview pl val of
          Just (Just pid) -> query ok . sign s $
            addTrack <*> playlist pid <*> artist "Ruby my dear" <*> track "Chazz" <*> ak <*> sk
          _ -> assertFailure (printf "bad JSON object: %s" (show val))

  testCreate = query pn . sign s $
    create <* title "Awesome playlist" <*> ak <*> sk


pl :: Query (Maybe Int64)
pl = key "playlists".key "playlist".values.key "id"._String.unpacked.to readMaybe

pn :: Query Text
pn = key "playlists".key "playlist".key "title"._String
