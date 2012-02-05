#!/usr/bin/env runhaskell

import Control.Monad ((<=<))

import Network.Lastfm.Core
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Chart as Chart

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getHypedArtistsExample :: IO ()
getHypedArtistsExample = do response <- Chart.getHypedArtists Nothing (Just (Limit 8)) apiKey
                            putStr "Top 8 hyped artists: "
                            case response of
                              Left e  -> print e
                              Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists"

getHypedTracksExample :: IO ()
getHypedTracksExample = do response <- Chart.getHypedTracks Nothing (Just (Limit 6)) apiKey
                           putStr "Top 6 hyped tracks: "
                           case response of
                             Left e  -> print e
                             Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "tracks"

getLovedTracksExample :: IO ()
getLovedTracksExample = do response <- Chart.getLovedTracks Nothing (Just (Limit 9)) apiKey
                           putStr "Top 9 most loved tracks: "
                           case response of
                             Left e  -> print e
                             Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "tracks"

getTopArtistsExample :: IO ()
getTopArtistsExample = do response <- Chart.getTopArtists Nothing (Just (Limit 4)) apiKey
                          putStr "Top 4 artists: "
                          case response of
                            Left e  -> print e
                            Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists"

getTopTagsExample :: IO ()
getTopTagsExample = do response <- Chart.getTopTags Nothing (Just (Limit 6)) apiKey
                       putStr "Top 6 tags: "
                       case response of
                         Left e  -> print e
                         Right r -> print (tags r)
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "tags"

getTopTracksExample :: IO ()
getTopTracksExample = do response <- Chart.getTopTracks Nothing (Just (Limit 7)) apiKey
                         putStr "Top 7 tracks: "
                         case response of
                           Left e  -> print e
                           Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "tracks"

main :: IO ()
main = do getHypedArtistsExample
          getHypedTracksExample
          getLovedTracksExample
          getTopArtistsExample
          getTopTagsExample
          getTopTracksExample
