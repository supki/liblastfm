{-# LANGUAGE FlexibleInstances #-}
module JSON.Event (private, public) where

import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Event
import Test.HUnit


p ∷ (Value → Parser b) → ByteString → Maybe b
p f xs = case AP.parse json xs of
  AP.Done _ j → case parse f j of
    Success v → Just v
    _ → Nothing
  _ → Nothing


(..:) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(..:) = fmap . fmap


instance Assertable (Either LastfmError Response) where
  assert = either (assertFailure . show) (const $ return ())
instance Assertable (Either LastfmError (Maybe a)) where
  assert α = either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust) α


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
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetAttendees = assert $
    p ga ..: getAttendees (Event 3142549) Nothing (Just $ Limit 2) ak

  testGetInfo = assert $
    p gi ..: getInfo (Event 3142549) ak

  testGetShouts = assert $
    p gs ..: getShouts (Event 3142549) Nothing (Just $ Limit 1) ak


gi, gs ∷ Value → Parser String
ga ∷ Value → Parser [String]
ga o = parseJSON o >>= (.: "attendees") >>= (.: "user") >>= mapM (.: "name")
gi o = parseJSON o >>= (.: "event") >>= (.: "venue") >>= (.: "location") >>= (.: "city")
gs o = parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= (.: "body")
