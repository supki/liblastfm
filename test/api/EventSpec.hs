{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module EventSpec (spec) where

import Control.Lens.Aeson
import Data.Text (Text)
import Network.Lastfm
import Network.Lastfm.Event
import Test.Hspec

import SpecHelper


spec :: Spec
spec = do
  it "Event.attend" $
    query_ . sign privateSecret $
      attend <*> event 3142549 <*> status Attending
      <*> privateAPIKey <*> privateSessionKey

  it "Event.share" $
    query_ . sign privateSecret $
      share <*> event 3142549 <*> recipient "liblastfm" <* message "Just listen!"
      <*> privateAPIKey <*> privateSessionKey

  it "Event.getAttendees" $
    query ga $
      getAttendees <*> event 3142549 <* limit 2 <*> publicKey

  it "Event.getInfo" $
    query gi $
      getInfo <*> event 3142549 <*> publicKey

  it "Event.getShouts" $
    query gs $
      getShouts <*> event 3142549 <* limit 1 <*> publicKey

ga, gi, gs :: Query Text
ga = key "attendees".key "user".values.key "name"._String
gi = key "event".key "venue".key "location".key "city"._String
gs = key "shouts".key "shout".key "body"._String
