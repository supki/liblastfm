{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Xml.EventSpec (spec) where

import Lastfm
import Lastfm.Event
import Test.Hspec
import Text.Xml.Lens

import SpecHelper


spec :: Spec
spec = do
  it "attend" $
    shouldHaveXml_ . privately $
      attend <*> event 3142549 <*> status Attending

  it "share" $
    shouldHaveXml_ . privately $
      share <*> event 3142549 <*> recipient "liblastfm" <* message "Just listen!"

  it "getAttendees" $
    publicly (getAttendees <*> event 3142549 <* limit 2)
   `shouldHaveXml`
    root.node "attendees".node "user".node "name".text

  it "getInfo" $
    publicly (getInfo <*> event 3142549)
   `shouldHaveXml`
    root.node "event".node "venue".node "location".node "country".text

  it "getShouts" $
    publicly (getShouts <*> event 3142549 <* limit 1)
   `shouldHaveXml`
    root.node "shouts".node "shout".node "body".text
