{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Event (auth, noauth) where

import Data.Aeson.Types
import Network.Lastfm
import Network.Lastfm.Event
import Test.Framework
import Test.Framework.Providers.HUnit

import Common


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Event.attend" testAttend
  , testCase "Event.share" testShare
  ]
 where
  testAttend = check ok . sign s $
    attend <*> event 3142549 <*> status Attending <*> ak <*> sk

  testShare = check ok . sign s $
    share <*> event 3142549 <*> recipient "liblastfm" <* message "Just listen!" <*> ak <*> sk


noauth :: Request JSON APIKey -> [Test]
noauth ak =
  [ testCase "Event.getAttendees" testGetAttendees
  , testCase "Event.getInfo" testGetInfo
  , testCase "Event.getShouts" testGetShouts
  ]
 where
  testGetAttendees = check ga $
    getAttendees <*> event 3142549 <* limit 2 <*> ak

  testGetInfo = check gi $
    getInfo <*> event 3142549 <*> ak

  testGetShouts = check gs $
    getShouts <*> event 3142549 <* limit 1 <*> ak


gi, gs :: Value -> Parser String
ga :: Value -> Parser [String]
ga o = parseJSON o >>= (.: "attendees") >>= (.: "user") >>= mapM (.: "name")
gi o = parseJSON o >>= (.: "event") >>= (.: "venue") >>= (.: "location") >>= (.: "city")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= (.: "body")
