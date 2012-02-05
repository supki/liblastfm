#!/usr/bin/env runhaskell

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
  where correction = liftM getContent . lookupChild "name" <=< lookupChild "artist" <=< lookupChild "correction" <=< lookupChild "corrections"

getEventsExample :: IO ()
getEventsExample = do response <- getEvents (Just (Artist "Meshuggah")) Nothing Nothing (Just (Limit 3)) Nothing Nothing apiKey
                      putStr "First event place: "
                      case response of
                        Left e  -> print e
                        Right r -> print (place r)
  where place = liftM getContent . lookupChild "name" <=< lookupChild "venue" <=< lookupChild "event" <=< lookupChild "events"

getImagesExample :: IO ()
getImagesExample = do response <- getImages (Just (Artist "Meshuggah")) Nothing Nothing (Just (Limit 3)) Nothing Nothing apiKey
                      putStr "First three images links: "
                      case response of
                        Left e  -> print e
                        Right r -> print (links r)
  where links = mapM (liftM (getContent) . lookupChild "url") <=< lookupChildren "image" <=< lookupChild "images"

main :: IO ()
main = do --addTagsExample (requires authorization)
          getCorrectionExample
          getEventsExample
          getImagesExample
          --getInfoExample
          --getPastEventsExample
          --getPodcastExample
          --getShoutsExample
          --getSimilarExample
          --getTagsExample
          --getTopAlbumsExample
          --getTopFansExample
          --getTopTagsExample
          --getTopTracksExample
          --removeTagExample
          --searchExample
          --shareExample
          -- shoutExample (requires authorization)
