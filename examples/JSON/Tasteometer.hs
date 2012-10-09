{-# LANGUAGE FlexibleInstances #-}
module JSON.Tasteometer (public) where

import Data.Maybe (isJust)
import Prelude hiding (compare)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Lazy as AP
import Data.ByteString.Lazy (ByteString)
import Network.Lastfm hiding (Value)
import Network.Lastfm.JSON.Tasteometer
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
public = return $ TestLabel "compare" $ TestCase testCompare
 where
  ak = APIKey "29effec263316a1f8a97f753caaa83e0"

  testCompare = assert $
    p c ..: compare (ValueUser $ User "smpcln") (ValueUser $ User "ingolfr") (Just $ Limit 10) ak


c ∷ Value → Parser String
c o = (parseJSON o >>= (.: "comparison") >>= (.: "result") >>= (.: "score"))
