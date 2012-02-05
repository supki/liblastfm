#!/usr/bin/env runhaskell

import Control.Applicative ((<$>))
import Control.Monad ((<=<), liftM)

import Network.Lastfm.Core
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Artist as Artist

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getCorrectionExample :: IO ()
getCorrectionExample = do response <- Artist.getCorrection (Artist "Meshugah") apiKey
                          putStr "Correction: "
                          case response of
                            Left e  -> print e
                            Right r -> print (correction r)
  where correction = getContent <=< lookupChild "name" <=< lookupChild "artist" <=< lookupChild "correction" <=< lookupChild "corrections"

getEventsExample :: IO ()
getEventsExample = do response <- Artist.getEvents (Just (Artist "Meshuggah")) Nothing Nothing Nothing (Just (Limit 3)) Nothing apiKey
                      putStr "First event place: "
                      case response of
                        Left e  -> print e
                        Right r -> print (place r)
  where place = getContent <=< lookupChild "name" <=< lookupChild "venue" <=< lookupChild "event" <=< lookupChild "events"

getImagesExample :: IO ()
getImagesExample = do response <- Artist.getImages (Just (Artist "Meshuggah")) Nothing Nothing Nothing (Just (Limit 3)) Nothing apiKey
                      putStr "First three images links: "
                      case response of
                        Left e  -> print e
                        Right r -> print (links r)
  where links = mapM (getContent <=< lookupChild "url") <=< lookupChildren "image" <=< lookupChild "images"

getInfoExample :: IO ()
getInfoExample = do response <- Artist.getInfo (Just (Artist "Meshuggah")) Nothing Nothing Nothing Nothing apiKey
                    putStr "Listeners count: "
                    case response of
                      Left e  -> print e
                      Right r -> print (listeners r)
  where listeners = getContent <=< lookupChild "listeners" <=< lookupChild "stats" <=< lookupChild "artist"

getPastEventsExample :: IO ()
getPastEventsExample = do response <- Artist.getPastEvents (Just (Artist "Meshugah")) Nothing (Just (Autocorrect True)) Nothing Nothing apiKey
                          putStr "All event artists: "
                          case response of
                            Left e  -> print e
                            Right r -> print (artists r)
  where artists = mapM getContent <=< lookupChildren "artist" <=< lookupChild "artists" <=< lookupChild "event" <=< lookupChild "events"

getPodcastExample :: IO ()
getPodcastExample = do response <- Artist.getPodcast (Just (Artist "Meshuggah")) Nothing Nothing apiKey
                       putStr "First channel description: "
                       case response of
                         Left e  -> print e
                         Right r -> print (description r)
  where description = getContent <=< lookupChild "description" <=< lookupChild "channel" <=< lookupChild "rss"

getShoutsExample :: IO ()
getShoutsExample = do response <- Artist.getShouts (Just (Artist "Meshuggah")) Nothing Nothing Nothing (Just (Limit 5)) apiKey
                      putStr "Last 5 shouts authors: "
                      case response of
                        Left e  -> print e
                        Right r -> print (authors r)
  where authors = mapM (getContent <=< lookupChild "author") <=< lookupChildren "shout" <=< lookupChild "shouts"

getSimilarExample :: IO ()
getSimilarExample = do response <- Artist.getSimilar (Just (Artist "Meshuggah")) Nothing Nothing (Just (Limit 7)) apiKey
                       putStr "7 similar artists: "
                       case response of
                         Left e  -> print e
                         Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "similarartists"

getTopAlbumsExample :: IO ()
getTopAlbumsExample = do response <- Artist.getTopAlbums (Just (Artist "Meshuggah")) Nothing Nothing Nothing (Just (Limit 3)) apiKey
                         putStr "3 most popular albums: "
                         case response of
                           Left e  -> print e
                           Right r -> print (albums r)
  where albums = mapM (getContent <=< lookupChild "name") <=< lookupChildren "album" <=< lookupChild "topalbums"

getTopFansExample :: IO ()
getTopFansExample = do response <- Artist.getTopFans (Just (Artist "Meshuggah")) Nothing Nothing apiKey
                       putStr "Top fans: "
                       case response of
                         Left e  -> print e
                         Right r -> print (fans r)
  where fans = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "topfans"

getTopTagsExample :: IO ()
getTopTagsExample = do response <- Artist.getTopTags (Just (Artist "Meshuggah")) Nothing Nothing apiKey
                       putStr "Top tags: "
                       case response of
                         Left e  -> print e
                         Right r -> print (tags r)
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "toptags"

getTopTracksExample :: IO ()
getTopTracksExample = do response <- Artist.getTopTracks (Just (Artist "Meshuggah")) Nothing Nothing Nothing (Just (Limit 10)) apiKey
                         putStr "10 most popular tracks: "
                         case response of
                           Left e  -> print e
                           Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks"

searchExample :: IO ()
searchExample = do response <- Artist.search (Artist "Mesh") Nothing (Just (Limit 12)) apiKey
                   putStr "12 search results for \"Mesh\" query: "
                   case response of
                     Left e  -> print e
                     Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artistmatches" <=< lookupChild "results"

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
          -- getTagsExample (requires authorization)
          getTopAlbumsExample
          getTopFansExample
          getTopTagsExample
          getTopTracksExample
          -- removeTagExample (requires authorization)
          searchExample
          -- shareExample (requires authorization)
          -- shoutExample (requires authorization)
