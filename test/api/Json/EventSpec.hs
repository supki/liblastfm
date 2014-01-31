{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Json.EventSpec (spec) where

import Data.Aeson.Lens
import Network.Lastfm
import Network.Lastfm.Event
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "attend" $
    shouldHaveJson_ . privately $
      attend <*> event 3142549 <*> status Attending

  it "share" $
    shouldHaveJson_ . privately $
      share <*> event 3142549 <*> recipient "liblastfm" <* message "Just listen!"

  it "getAttendees" $
    publicly (getAttendees <*> event 3142549 <* limit 2)
   `shouldHaveJson`
    key "attendees".key "user".values.key "name"._String

  it "getInfo" $
    publicly (getInfo <*> event 3142549)
   `shouldHaveJson`
    key "event".key "venue".key "location".key "city"._String

  it "getShouts" $
    publicly (getShouts <*> event 3142549 <* limit 1)
   `shouldHaveJson`
    key "shouts".key "shout".key "body"._String
