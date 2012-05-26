module EVenue (common, auth) where

import Control.Monad ((<=<), liftM)

import Network.Lastfm.Types
import qualified Network.Lastfm.XML.Venue as Venue

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getEvents :: IO ()
getEvents = parse r f "Venue name"
  where r = Venue.getEvents (Venue 9163107) Nothing apiKey
        f = fmap return . content <=< tag "name" <=< tag "venue" <=< tag "event" <=< tag "events"

getPastEvents :: IO ()
getPastEvents = parse r f "Artists from 10 last events"
  where r = Venue.getPastEvents (Venue 9163107) Nothing Nothing (Just $ Limit 10) apiKey
        f = liftM concat . mapM (mapM content <=< tags "artist" <=< tag "artists") <=< tags "event" <=< tag "events"

search :: IO ()
search = parse r f "Venue ids"
  where r = Venue.search (Venuename "Arena") Nothing Nothing Nothing apiKey
        f = mapM (content <=< tag "id") <=< tags "venue" <=< tag "venuematches" <=< tag "results"

common :: IO ()
common = do getEvents
            getPastEvents
            search

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth _ _ _ = return ()
