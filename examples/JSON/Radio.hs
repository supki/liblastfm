{-# LANGUAGE FlexibleInstances #-}
module JSON.Radio (private, public) where

import Data.Maybe (isJust)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Radio
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
  assert α = either processError (assertBool "Cannot parse JSON" . isJust) α
   where
    processError AuthenticationFailed = return ()
    processError e = assertFailure $ show e


private ∷ APIKey → SessionKey → Secret → [Test]
private ak sk s =
  [ TestLabel "Radio.tune" $ TestCase testTune
  , TestLabel "Radio.getPlaylist" $ TestCase testGetPlaylist
  ]
 where
  testGetPlaylist = assert $
    p gp ..: getPlaylist Nothing Nothing Nothing M2 B64 ak sk s

  testTune = assert $
    p t ..: tune Nothing (Station "lastfm://artist/Merzbow/similarartists") ak sk s


public ∷ [Test]
public = return $ TestLabel "Radio.search" $ TestCase testSearch
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testSearch = assert $
    p se ..: search (Name "dubstep") ak


gp ∷ Value → Parser [String]
se, t ∷ Value → Parser String
gp o = (parseJSON o >>= (.: "playlist") >>= (.: "trackList") >>= (.: "track") >>= mapM (.: "title"))
se o = (parseJSON o >>= (.: "stations") >>= (.: "station") >>= (.: "name"))
t o = (parseJSON o >>= (.: "station") >>= (.: "url"))
