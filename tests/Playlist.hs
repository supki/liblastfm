{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Playlist (auth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Playlist
import Network.Lastfm.User
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Playlist.create"   testCreate -- Order matters.
  , testCase "Playlist.addTrack" testAddTrack
  ]
 where
  ak' = "29effec263316a1f8a97f753caaa83e0"

  -- Poo
  testAddTrack =
    do r <- lastfm $ getPlaylists <*> user "liblastfm" <*> apiKey ak' <* json
       case parseMaybe pl =<< either (const Nothing) Just r of
         Nothing -> error "M"
         Just r' -> let pid = read $ head r' in check ok . sign s $
           addTrack <*> playlist pid <*> artist "Ruby my dear" <*> track "Chazz" <*> ak <*> sk

  testCreate = check pn . sign s $
    create <* title "Awesome playlist" <*> ak <*> sk


pl :: Value -> Parser [String]
pl o = parseJSON o >>= (.: "playlists") >>= (.: "playlist") >>= mapM (.: "id")

pn :: Value -> Parser String
pn o = parseJSON o >>= (.: "playlists") >>= (.: "playlist") >>= (.: "title")
