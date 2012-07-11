{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module EAlbum (common, auth) where

import Control.Applicative ((<$>), empty)
import Data.Char (isSpace)
import Data.Monoid ((<>))

import Data.Aeson
import Network.Lastfm
import Network.Lastfm.JSON.Album


main ∷ IO ()
main =
  do (a, sk, s) ← getConfig "../.lastfm.conf"
     common
     auth a sk s


getConfig ∷ FilePath → IO (APIKey, SessionKey, Secret)
getConfig fp = do
  (apiKey:sessionKey:secret:_) ← map (drop 1 . dropWhile (/= '=') . filter (not . isSpace)) . lines <$> readFile fp
  return (APIKey apiKey, SessionKey sessionKey, Secret secret)


common ∷ IO ()
common =
  do exampleGetBuylinks ak
     return ()
 where
  ak = APIKey "b25b959554ed76058ac220b7b2e0a026"

auth ∷ APIKey → SessionKey → Secret → IO ()
auth ak sk s =
  do exampleAddTags ak sk s
     return ()


exampleAddTags ∷ APIKey → SessionKey → Secret → IO ()
exampleAddTags ak sk s =
  do r ← addTags (Artist "Pink Floyd", Album "The Wall") [Tag "70s", Tag "awesome"] ak sk s
     case r of
       Left e → putStrLn $ "addTags: ERROR! " <> show e
       Right _ → putStrLn "addTags: OK!"


exampleGetBuylinks ∷ APIKey → IO ()
exampleGetBuylinks ak =
  do r ← getBuyLinks (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Country "United Kingdom") ak
     case r of
       Left e → putStrLn $ "getBuyLinks: ERROR! " <> show e
       Right r' → putStrLn $ "getBuyLinks: OK! " <> show (decode r' ∷ Maybe GBL)


newtype GBL = GBL [String] deriving Show


instance FromJSON GBL where
  parseJSON (Object o) =
    GBL <$> ((o .: "affiliations") >>= (.: "physicals") >>= (.: "affiliation") >>= mapM (.: "supplierName"))
  parseJSON _ = empty
