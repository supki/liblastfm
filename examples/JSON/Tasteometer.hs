{-# LANGUAGE FlexibleInstances #-}
module JSON.Tasteometer (public) where

import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Prelude hiding (compare)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Tasteometer
import Test.HUnit


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


public ∷ [Test]
public = return $ TestLabel "compare" $ TestCase testCompare
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

  testCompare = assert
    (compare (ValueUser $ User "smpcln") (ValueUser $ User "ingolfr") (Just $ Limit 10) ak, decode ∷ Response → Maybe C)


newtype C = C String


instance FromJSON C where
  parseJSON o = C <$> (parseJSON o >>= (.: "comparison") >>= (.: "result") >>= (.: "score"))
