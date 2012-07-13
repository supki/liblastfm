{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Artist (main) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Prelude hiding (GT)

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Event


main ∷ IO ()
main = common >> auth


common ∷ IO ()
common = mapM_ ($ ak)
  [ exampleGetAttendees
  , exampleGetInfo
  , exampleGetShouts
  ]
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"


auth ∷ IO ()
auth =
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


exampleGetAttendees ∷ APIKey → IO ()
exampleGetAttendees ak =
  do r ← getAttendees (Event 3142549) Nothing (Just $ Limit 2) ak
     putStrLn $ case r of
       Left e → "getAttendees: ERROR! " <> show e
       Right r' → "getAttendees: OK! First 2 attendees: " <> show (unGA <$> decode r')


exampleGetInfo ∷ APIKey → IO ()
exampleGetInfo ak =
  do r ← getInfo (Event 3142549) ak
     putStrLn $ case r of
       Left e → "getInfo: ERROR! " <> show e
       Right r' → "getInfo: OK! Event city: " <> show (unGI <$> decode r')


exampleGetShouts ∷ APIKey → IO ()
exampleGetShouts ak =
  do r ← getShouts (Event 3142549) Nothing (Just $ Limit 1) ak
     putStrLn $ case r of
       Left e → "getShouts: ERROR! " <> show e
       Right r' → "getShouts: OK! First shout: " <> show (unGS <$> decode r')


exampleShare ∷ APIKey → SessionKey → Secret → IO ()
exampleShare ak sk s =
  do r ← share (Event 3142549) (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s
     putStrLn $ case r of
       Left e → "share: ERROR! " <> show e
       Right _ → "share: OK!"


newtype GA = GA { unGA ∷ [String] } deriving Show
newtype GI = GI { unGI ∷ String } deriving Show
newtype GS = GS { unGS ∷ String } deriving Show


instance FromJSON GA where
  parseJSON o = GA <$> (parseJSON o >>= (.: "attendees") >>= (.: "user") >>= mapM (.: "name"))
instance FromJSON GI where
  parseJSON o = GI <$> (parseJSON o >>= (.: "event") >>= (.: "venue") >>= (.: "location") >>= (.: "city"))
instance FromJSON GS where
  parseJSON o = GS <$> (parseJSON o >>= (.: "shouts") >>= (.: "shout") >>= (.: "body"))
