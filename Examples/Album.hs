#!/usr/bin/env runhaskell

import Control.Monad ((<=<))

import Network.Lastfm.Core
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Album as Album

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getBuylinksExample :: IO ()
getBuylinksExample = do response <- Album.getBuyLinks (Just (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing (Country "United Kingdom") apiKey
                        putStr "Download suppliers: "
                        case response of
                          Left e  -> print e
                          Right r -> print (suppliers r)
  where suppliers = mapM (getContent <=< lookupChild "supplierName") <=< lookupChildren "affiliation" <=< lookupChild "downloads" <=< lookupChild "affiliations"

getInfoExample :: IO ()
getInfoExample = do response <- Album.getInfo (Just (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing Nothing Nothing apiKey
                    putStr "Top 5 tags: "
                    case response of
                      Left e  -> print e
                      Right r -> print (suppliers r)
  where suppliers = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "toptags" <=< lookupChild "album"

getShoutsExample :: IO ()
getShoutsExample = do response <- Album.getShouts (Just (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing Nothing (Just (Limit 7)) apiKey
                      putStr "Last 7 shouts: "
                      case response of
                        Left e  -> print e
                        Right r -> print (shouts r)
  where shouts = mapM (getContent <=< lookupChild "body") <=< lookupChildren "shout" <=< lookupChild "shouts"

getTopTagsExample :: IO ()
getTopTagsExample = do response <- Album.getTopTags (Just (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing apiKey
                       putStr "Top tags counts: "
                       case response of
                         Left e  -> print e
                         Right r -> print (counts r)
  where counts = mapM (getContent <=< lookupChild "count") <=< lookupChildren "tag" <=< lookupChild "toptags"

searchExample :: IO ()
searchExample = do response <- Album.search (Album "wall") Nothing (Just (Limit 5)) apiKey
                   putStr "5 search results for \"wall\" query: "
                   case response of
                     Left e  -> print e
                     Right r -> print (albums r)
  where albums = mapM (getContent <=< lookupChild "name") <=< lookupChildren "album" <=< lookupChild "albummatches" <=< lookupChild "results"

main :: IO ()
main = do -- addTagsExample (requires authorization)
          getBuylinksExample
          getInfoExample
          getShoutsExample
          -- getTagsExample (requires authorization)
          getTopTagsExample
          -- removeTagExample (requires authorization)
          searchExample
          -- shareExample (requires authorization)
