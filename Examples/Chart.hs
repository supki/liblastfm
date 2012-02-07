#!/usr/bin/env runhaskell

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Chart as Chart

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getHypedArtists :: IO ()
getHypedArtists = do response <- Chart.getHypedArtists Nothing (Just (Limit 8)) apiKey
                     putStr "Top 8 hyped artists: "
                     case response of
                       Left e  -> print e
                       Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists" <=< wrap

getHypedTracks :: IO ()
getHypedTracks = do response <- Chart.getHypedTracks Nothing (Just (Limit 6)) apiKey
                    putStr "Top 6 hyped tracks: "
                    case response of
                      Left e  -> print e
                      Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "tracks" <=< wrap

getLovedTracks :: IO ()
getLovedTracks = do response <- Chart.getLovedTracks Nothing (Just (Limit 9)) apiKey
                    putStr "Top 9 most loved tracks: "
                    case response of
                      Left e  -> print e
                      Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "tracks" <=< wrap

getTopArtists :: IO ()
getTopArtists = do response <- Chart.getTopArtists Nothing (Just (Limit 4)) apiKey
                   putStr "Top 4 artists: "
                   case response of
                     Left e  -> print e
                     Right r -> print (artists r)
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artists" <=< wrap

getTopTags :: IO ()
getTopTags = do response <- Chart.getTopTags Nothing (Just (Limit 6)) apiKey
                putStr "Top 6 tags: "
                case response of
                  Left e  -> print e
                  Right r -> print (tags r)
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "tags" <=< wrap

getTopTracks :: IO ()
getTopTracks = do response <- Chart.getTopTracks Nothing (Just (Limit 7)) apiKey
                  putStr "Top 7 tracks: "
                  case response of
                    Left e  -> print e
                    Right r -> print (tracks r)
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "tracks" <=< wrap

main :: IO ()
main = do getHypedArtists
          getHypedTracks
          getLovedTracks
          getTopArtists
          getTopTags
          getTopTracks
