{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Playlist (auth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Playlist
import Network.Lastfm.User
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ testCase "Playlist.create"   testCreate -- Order matters.
  , testCase "Playlist.addTrack" testAddTrack
  ]
 where
  testAddTrack =
    do r ← lastfm $ getPlaylists <*> user "liblastfm" <*> apiKey ak <* json
       case parseMaybe pl =<< r of
         Nothing → error "M"
         Just r' → let pid = read $ head r' in check ok . sign s $
           addTrack <*> playlist pid <*> artist "Ruby my dear" <*> track "Chazz"
            <*> apiKey ak <*> sessionKey sk

  testCreate = check pn . sign s $
    create <* title "Awesome playlist" <*> apiKey ak <*> sessionKey sk


pl ∷ Value → Parser [String]
pl o = parseJSON o >>= (.: "playlists") >>= (.: "playlist") >>= mapM (.: "id")

pn ∷ Value → Parser String
pn o = parseJSON o >>= (.: "playlists") >>= (.: "playlist") >>= (.: "title")
