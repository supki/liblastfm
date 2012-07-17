{-# LANGUAGE FlexibleInstances #-}
module JSON.Radio (private, public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Radio
import Test.HUnit


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either processError (assertBool "Cannot parse JSON" . isJust . β)
   where
    processError ∷ LastfmError → Assertion
    processError AuthenticationFailed = return ()
    processError e = assertFailure $ show e


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "tune" $ TestCase testTune
  , TestLabel "getPlaylist" $ TestCase testGetPlaylist
  ]
 where
  testGetPlaylist = assert
    (getPlaylist Nothing Nothing Nothing M2 B64 ak sk s, decode ∷ Response → Maybe GP)

  testTune = assert
    (tune Nothing (Station "lastfm://artist/Merzbow/similarartists") ak sk s, decode ∷ Response → Maybe T)


public ∷ [Test]
public = return $ TestLabel "search" $ TestCase testSearch
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testSearch = assert
    (search (Name "dubstep") ak, decode ∷ Response → Maybe SE)


newtype GP = GP [String]
newtype SE = SE String
newtype T = T String


instance FromJSON GP where
  parseJSON o = GP <$> (parseJSON o >>= (.: "playlist") >>= (.: "trackList") >>= (.: "track") >>= mapM (.: "title"))
instance FromJSON SE where
  parseJSON o = SE <$> (parseJSON o >>= (.: "stations") >>= (.: "station") >>= (.: "name"))
instance FromJSON T where
  parseJSON o = T <$> (parseJSON o >>= (.: "station") >>= (.: "url"))
