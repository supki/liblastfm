{-# LANGUAGE FlexibleInstances #-}
module JSON.Venue (public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Venue
import Test.HUnit


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


public ∷ [Test]
public =
  [ TestLabel "getEvents" $ TestCase testGetEvents
  , TestLabel "getPastEvents" $ TestCase testGetPastEvents
  , TestLabel "search" $ TestCase testSearch
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testGetEvents = assert
    (getEvents (Venue 9163107) Nothing ak, decode ∷ Response → Maybe GE)

  testGetPastEvents = assert
    (getPastEvents (Venue 9163107) Nothing Nothing (Just $ Limit 2) ak, decode ∷ Response → Maybe GPE)

  testSearch = assert
    (search (Venuename "Arena") Nothing Nothing Nothing ak, decode ∷ Response → Maybe SE)


newtype GE = GE [String] deriving Show
newtype GPE = GPE [String] deriving Show
newtype SE = SE [String] deriving Show


instance FromJSON GE where
  parseJSON o = GE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\t → (t .: "venue") >>= (.: "name")))
instance FromJSON GPE where
  parseJSON o = GPE <$> (parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\t → (t .: "artists") >>= (.: "artist")))
instance FromJSON SE where
  parseJSON o = SE <$> (parseJSON o >>= (.: "results") >>= (.: "venuematches") >>= (.: "venue") >>= mapM (.: "id"))
