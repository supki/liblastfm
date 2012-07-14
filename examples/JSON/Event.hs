{-# LANGUAGE FlexibleInstances #-}
module JSON.Event (private, public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Event
import Test.HUnit


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())
instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "attend" $ TestCase testAttend
  , TestLabel "share" $ TestCase testShare
  ]
 where
  testAttend = assert $
    attend (Event 3142549) Maybe ak sk s

  testShare = assert $
    share (Event 3142549) (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s


public ∷ [Test]
public =
  [ TestLabel "getAttendees" $ TestCase testGetAttendees
  , TestLabel "getInfo" $ TestCase testGetInfo
  , TestLabel "getShouts" $ TestCase testGetShouts
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetAttendees = assert
    (getAttendees (Event 3142549) Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GA)

  testGetInfo = assert
    (getInfo (Event 3142549) ak, decode ∷ Response → Maybe GI)

  testGetShouts = assert
    (getShouts (Event 3142549) Nothing (Just $ Limit 1) ak, decode ∷ Response → Maybe GS)


newtype GA = GA [String] deriving Show
newtype GI = GI String deriving Show
newtype GS = GS String deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "attendees") >>= (.: "user") >>= mapM (.: "name"))
instance FromJSON GI where
  parseJSON o = GI <$> (parseJSON o >>= (.: "event") >>= (.: "venue") >>= (.: "location") >>= (.: "city"))
instance FromJSON GS where
  parseJSON o = GS <$> (parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= (.: "body"))
