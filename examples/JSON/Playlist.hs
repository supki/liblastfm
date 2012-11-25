{-# LANGUAGE FlexibleInstances #-}
module JSON.Playlist (private) where

import Control.Applicative ((<$>))

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Playlist
import Network.Lastfm.JSON.User
import Test.HUnit


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "Playlist.create" $ TestCase testCreate -- Order matters.
  , TestLabel "Playlist.addTrack" $ TestCase testAddTrack
  ]
 where
  testAddTrack =
    do r ← getPlaylists (User "liblastfm") ak
       case r of
         Left e → assertFailure $ "User.getPlaylists has failed: " ++ show e
         Right r' → case decode r' of
                      Nothing → assertFailure "User.getPlaylists JSON parsing has failed"
                      Just (PL r'') → let pid = read $ head r'' in assert $
                        addTrack (Playlist pid) (Artist "Ruby my dear") (Track "Chazz") ak sk s

  testCreate = assert $
    create (Just $ Title "Awesome playlist") Nothing ak sk s


newtype PL = PL [String] deriving Show


instance FromJSON PL where
  parseJSON o = PL <$> (parseJSON o >>= (.: "playlists") >>= (.: "playlist") >>= mapM (.: "id"))
