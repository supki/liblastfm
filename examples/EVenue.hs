module EVenue (start) where

import Control.Monad ((<=<), liftM)

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Venue as Venue

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getEvents :: IO ()
getEvents = do response <- Venue.getEvents (Venue 9163107) Nothing apiKey
               putStr "Venue name: "
               case response of
                 Left e  -> print e
                 Right r -> print $ venue r
               putStrLn ""
  where venue = getContent <=< lookupChild "name" <=< lookupChild "venue" <=< lookupChild "event" <=< lookupChild "events" <=< wrap

getPastEvents :: IO ()
getPastEvents = do response <- Venue.getPastEvents (Venue 9163107) Nothing Nothing (Just $ Limit 10) apiKey
                   putStr "Artists from 10 last events: "
                   case response of
                     Left e  -> print e
                     Right r -> print $ venue r
                   putStrLn ""
  where venue = liftM concat . mapM (mapM getContent <=< lookupChildren "artist" <=< lookupChild "artists") <=< lookupChildren "event" <=< lookupChild "events" <=< wrap

search :: IO ()
search = do response <- Venue.search (Name "Arena") Nothing Nothing Nothing apiKey
            putStr "venue ids: "
            case response of
              Left e  -> print e
              Right r -> print $ venues r
            putStrLn ""
  where venues = mapM (getContent <=< lookupChild "id") <=< lookupChildren "venue" <=< lookupChild "venuematches" <=< lookupChild "results" <=< wrap

start :: IO ()
start = do getEvents
           getPastEvents
           search 
