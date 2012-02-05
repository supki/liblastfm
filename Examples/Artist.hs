#!/usr/bin/env runhaskell

import Control.Applicative ((<$>))
import Control.Monad ((<=<), liftM)

import Network.Lastfm.API.Artist
import Network.Lastfm.Core
import Network.Lastfm.Types

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getCorrectionExample :: IO ()
getCorrectionExample = do response <- getCorrection (Artist "Meshugah") apiKey
                          putStr "Correction: "
                          case response of
                            Left e  -> print e
                            Right r -> print (correction r)
  where correction = getContent <=< lookupChild "name" <=< lookupChild "artist" <=< lookupChild "correction" <=< lookupChild "corrections"

getEventsExample :: IO ()
getEventsExample = do response <- getEvents (Just (Artist "Meshuggah")) Nothing Nothing (Just (Limit 3)) Nothing Nothing apiKey
                      putStr "First event place: "
                      case response of
                        Left e  -> print e
                        Right r -> print (place r)
  where place = getContent <=< lookupChild "name" <=< lookupChild "venue" <=< lookupChild "event" <=< lookupChild "events"

getImagesExample :: IO ()
getImagesExample = do response <- getImages (Just (Artist "Meshuggah")) Nothing Nothing (Just (Limit 3)) Nothing Nothing apiKey
                      putStr "First three images links: "
                      case response of
                        Left e  -> print e
                        Right r -> print (links r)
  where links = mapM (getContent <=< lookupChild "url") <=< lookupChildren "image" <=< lookupChild "images"

getInfoExample :: IO ()
getInfoExample = do response <- getInfo (Just (Artist "Meshuggah")) Nothing Nothing Nothing Nothing apiKey
                    putStr "Listeners count: "
                    case response of
                      Left e  -> print e
                      Right r -> print (listeners r)
  where listeners = getContent <=< lookupChild "listeners" <=< lookupChild "stats" <=< lookupChild "artist"

getPastEventsExample :: IO ()
getPastEventsExample = do response <- getPastEvents (Just (Artist "Meshugah")) Nothing Nothing (Just (Autocorrect True)) Nothing apiKey
                          putStr "All event artists: "
                          case response of
                            Left e  -> print e
                            Right r -> print (artists r)
  where artists = mapM getContent <=< lookupChildren "artist" <=< lookupChild "artists" <=< lookupChild "event" <=< lookupChild "events"

getPodcastExample :: IO ()
getPodcastExample = do response <- getPodcast (Just (Artist "Meshuggah")) Nothing Nothing apiKey
                       putStr "First channel description: "
                       case response of
                         Left e  -> print e
                         Right r -> print (description r)
  where description = getContent <=< lookupChild "description" <=< lookupChild "channel" <=< lookupChild "rss"

getShoutsExample :: IO ()
getShoutsExample = do response <- getShouts (Just (Artist "Meshuggah")) Nothing (Just (Limit 5)) Nothing Nothing apiKey
                      putStr "Last 5 shouts authors: "
                      case response of
                        Left e  -> print e
                        Right r -> print (authors r)
  where authors = mapM (getContent <=< lookupChild "author") <=< lookupChildren "shout" <=< lookupChild "shouts"

getSimilarExample :: IO ()
getSimilarExample = do response <- getSimilar (Just (Artist "Meshuggah")) Nothing (Just (Limit 7)) Nothing apiKey
                       putStr "7 similar artists: "
                       case response of
                         Left e  -> print e
                         Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "similarartists"

main :: IO ()
main = do -- addTagsExample (requires authorization)
          getCorrectionExample
          getEventsExample
          getImagesExample
          getInfoExample
          getPastEventsExample
          getPodcastExample
          getShoutsExample
          getSimilarExample
          -- getTagsExample
          -- getTopAlbumsExample
          -- getTopFansExample
          -- getTopTagsExample
          -- getTopTracksExample
          -- removeTagExample
          -- searchExample
          -- shareExample
          -- shoutExample (requires authorization)
