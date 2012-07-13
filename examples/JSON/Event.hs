{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module JSON.Event (tests) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Event
import Test.HUnit


main ∷ IO ()
main =
  do (ak, sk, s) ← getConfig "../.lastfm.conf"
     mapM_ (\f → f ak sk s)
       [ exampleAttend
       , exampleShare
    {- , for shout see User.shout example -}
       ]
 where
  getConfig fp =
    do (apiKey:sessionKey:secret:_) ← map (drop 1 . dropWhile (/= '=') . filter (not . isSpace)) . lines <$> readFile fp
       return (APIKey apiKey, SessionKey sessionKey, Secret secret)


exampleAttend ∷ APIKey → SessionKey → Secret → IO ()
exampleAttend ak sk s =
  do r ← attend (Event 3142549) Maybe ak sk s
     putStrLn $ case r of
       Left e → "attend: ERROR! " <> show e
       Right _ → "attend: OK!"


exampleShare ∷ APIKey → SessionKey → Secret → IO ()
exampleShare ak sk s =
  do r ← share (Event 3142549) (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s
     putStrLn $ case r of
       Left e → "share: ERROR! " <> show e
       Right _ → "share: OK!"


instance FromJSON α ⇒ Assertable (Lastfm Response, Response → Maybe α) where
  assert (α, β) = α >>= either (assertFailure . show) (assertBool "Cannot parse JSON" . isJust . β)


tests ∷ [Test]
tests =
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
