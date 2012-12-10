{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Event (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Event
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ testCase "Event.attend" testAttend
  , testCase "Event.share" testShare
  ]
 where
  testAttend = check ok . sign s $
    attend 3142549 Attending <> apiKey ak <> sessionKey sk

  testShare = check ok . sign s $
    share 3142549 "liblastfm" <> message "Just listen!" <> apiKey ak <> sessionKey sk


noauth ∷ [Test]
noauth =
  [ testCase "Event.getAttendees" testGetAttendees
  , testCase "Event.getInfo" testGetInfo
  , testCase "Event.getShouts" testGetShouts
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetAttendees = check ga $
    getAttendees 3142549 <> limit 2 <> apiKey ak

  testGetInfo = check gi $
    getInfo 3142549 <> apiKey ak

  testGetShouts = check gs $
    getShouts 3142549 <> limit 1 <> apiKey ak


gi, gs ∷ Value → Parser String
ga ∷ Value → Parser [String]
ga o = parseJSON o >>= (.: "attendees") >>= (.: "user") >>= mapM (.: "name")
gi o = parseJSON o >>= (.: "event") >>= (.: "venue") >>= (.: "location") >>= (.: "city")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= (.: "body")
