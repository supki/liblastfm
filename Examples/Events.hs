#!/usr/bin/env runhaskell

import Control.Monad ((<=<))
import Data.Maybe (fromMaybe)

import Network.Lastfm.Core
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Event as Event

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

getAttendeesExample :: IO ()
getAttendeesExample = do response <- Event.getAttendees (Event 3142549) Nothing (Just (Limit 10)) apiKey
                         putStr "First 10 attendees: "
                         case response of
                           Left e  -> print e
                           Right r -> print (attendees r)
  where attendees = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "attendees"

getInfoExample :: IO ()
getInfoExample = do response <- Event.getInfo (Event 3142549) apiKey
                    putStr "City: "
                    case response of
                      Left e  -> print e
                      Right r -> print (city r)
  where city = getContent <=< lookupChild "city" <=< lookupChild "location" <=< lookupChild "venue" <=< lookupChild "event"

getShoutsExample :: IO ()
getShoutsExample = do response <- Event.getShouts (Event 3142549) Nothing (Just (Limit 8)) apiKey
                      putStrLn "First 8 shouts:"
                      case response of
                        Left e  -> print e
                        Right r -> mapM_ (\s -> putStrLn $ "* " ++ s) . fromMaybe [] . shouts $ r
  where shouts = mapM (getContent <=< lookupChild "body") <=< lookupChildren "shout" <=< lookupChild "shouts"

main :: IO ()
main = do -- attendExample (requires authorization)
          getAttendeesExample
          getInfoExample
          getShoutsExample
          -- shareExample (requires authorization)
          -- shoutExample (requires authorization)
