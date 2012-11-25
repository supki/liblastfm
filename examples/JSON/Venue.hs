{-# LANGUAGE FlexibleInstances #-}
module JSON.Venue (public) where

import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Venue
import Test.HUnit


p ∷ (Value → Parser b) → ByteString → Maybe b
p f xs = case AP.parse json xs of
  AP.Done _ j → case parse f j of
    Success v → Just v
    _ → Nothing
  _ → Nothing


(..:) ∷ (Functor f, Functor g) ⇒ (a → b) → f (g a) → f (g b)
(..:) = fmap . fmap


instance Assertable (Either LastfmError (Maybe a)) where
  assert α = either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust) α


public ∷ [Test]
public =
  [ TestLabel "Venue.getEvents" $ TestCase testGetEvents
  , TestLabel "Venue.getPastEvents" $ TestCase testGetPastEvents
  , TestLabel "Venue.search" $ TestCase testSearch
  ]
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testGetEvents = assert $
    p ge ..: getEvents (Venue 9163107) Nothing ak

  testGetPastEvents = assert $
    p gpe ..: getPastEvents (Venue 9163107) Nothing Nothing (Just $ Limit 2) ak

  testSearch = assert $
    p se ..: search (Venuename "Arena") Nothing Nothing Nothing ak


ge, gpe, se ∷ Value → Parser [String]
ge o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\t → (t .: "venue") >>= (.: "name"))
gpe o = parseJSON o >>= (.: "events") >>= (.: "event") >>= mapM (\t → (t .: "artists") >>= (.: "artist"))
se o = parseJSON o >>= (.: "results") >>= (.: "venuematches") >>= (.: "venue") >>= mapM (.: "id")
