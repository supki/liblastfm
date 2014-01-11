{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Event (auth, noauth) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Event
import Test.Framework
import Test.Framework.Providers.HUnit

import Helper


auth :: Request JSON APIKey -> Request JSON SessionKey -> Secret -> [Test]
auth ak sk s =
  [ testCase "Event.attend" testAttend
  , testCase "Event.share" testShare
  ]
 where
  testAttend = query ok . sign s $
    attend <*> event 3142549 <*> status Attending <*> ak <*> sk

  testShare = query ok . sign s $
    share <*> event 3142549 <*> recipient "liblastfm" <* message "Just listen!" <*> ak <*> sk


noauth :: Request JSON APIKey -> [Test]
noauth ak =
  [ testCase "Event.getAttendees" testGetAttendees
  , testCase "Event.getInfo" testGetInfo
  , testCase "Event.getShouts" testGetShouts
  ]
 where
  testGetAttendees = query ga $
    getAttendees <*> event 3142549 <* limit 2 <*> ak

  testGetInfo = query gi $
    getInfo <*> event 3142549 <*> ak

  testGetShouts = query gs $
    getShouts <*> event 3142549 <* limit 1 <*> ak


ga, gi, gs :: Query Text
ga = key "attendees".key "user".values.key "name"._String
gi = key "event".key "venue".key "location".key "city"._String
gs = key "shouts".key "shout".key "body"._String
