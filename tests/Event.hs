{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Event (auth, noauth) where

import Data.Aeson.Types
import Data.Text.Lazy (Text)
import Network.Lastfm
import Network.Lastfm.Event
import Test.HUnit

import Common


auth ∷ Text → Text → Text → [Test]
auth ak sk s =
  [ TestLabel "Event.attend" $ TestCase testAttend
  , TestLabel "Event.share" $ TestCase testShare
  ]
 where
  testAttend = assert $ parse ok <:> lastfm (sign s $
    attend 3142549 Attending <> apiKey ak <> sessionKey sk <> json)

  testShare = assert $ parse ok <:> lastfm (sign s $
    share 3142549 "liblastfm" <> message "Just listen!" <> sessionKey sk <> apiKey ak <> json)


noauth ∷ [Test]
noauth =
  [ TestLabel "Event.getAttendees" $ TestCase testGetAttendees
  , TestLabel "Event.getInfo" $ TestCase testGetInfo
  , TestLabel "Event.getShouts" $ TestCase testGetShouts
  ]
 where
  ak = "29effec263316a1f8a97f753caaa83e0"

  testGetAttendees = assert $ parse ga <:> lastfm (
    getAttendees 3142549 <> limit 2 <> apiKey ak <> json)

  testGetInfo = assert $ parse gi <:> lastfm (
    getInfo 3142549 <> apiKey ak <> json)

  testGetShouts = assert $ parse gs <:> lastfm (
    getShouts 3142549 <> limit 1 <> apiKey ak <> json)


gi, gs ∷ Value → Parser String
ga ∷ Value → Parser [String]
ga o = parseJSON o >>= (.: "attendees") >>= (.: "user") >>= mapM (.: "name")
gi o = parseJSON o >>= (.: "event") >>= (.: "venue") >>= (.: "location") >>= (.: "city")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= (.: "body")
