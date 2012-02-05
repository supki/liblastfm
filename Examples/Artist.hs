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
                            Right r -> print (listeners r)
  where listeners = mapM getContent <=< lookupChildren "artist" <=< lookupChild "artists" <=< lookupChild "event" <=< lookupChild "events"

main :: IO ()
main = do -- addTagsExample (requires authorization)
          getCorrectionExample
          getEventsExample
          getImagesExample
          getInfoExample
          getPastEventsExample
          -- getPodcastExample
          -- getShoutsExample
          -- getSimilarExample
          -- getTagsExample
          -- getTopAlbumsExample
          -- getTopFansExample
          -- getTopTagsExample
          -- getTopTracksExample
          -- removeTagExample
          -- searchExample
          -- shareExample
          -- shoutExample (requires authorization)
