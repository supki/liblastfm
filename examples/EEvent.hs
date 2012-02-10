module EEvent (common, auth) where

import Control.Monad ((<=<))
import Data.Maybe (fromMaybe)

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Event as Event

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
event = Event 3142549

attend :: APIKey -> SessionKey -> IO ()
attend apiKey sessionKey = do response <- Event.attend event Maybe apiKey sessionKey
                              case response of
                                Left e  -> print e
                                Right () -> return ()

getAttendees :: IO ()
getAttendees = do response <- Event.getAttendees event Nothing (Just $ Limit 10) apiKey
                  putStr "First 10 attendees: "
                  case response of
                    Left e  -> print e
                    Right r -> print (attendees r)
                  putStrLn ""
  where attendees = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "attendees" <=< wrap

getInfo :: IO ()
getInfo = do response <- Event.getInfo event apiKey
             putStr "City: "
             case response of
               Left e  -> print e
               Right r -> print (city r)
             putStrLn ""
  where city = getContent <=< lookupChild "city" <=< lookupChild "location" <=< lookupChild "venue" <=< lookupChild "event" <=< wrap

getShouts :: IO ()
getShouts = do response <- Event.getShouts event Nothing (Just $ Limit 8) apiKey
               putStrLn "First 8 shouts:"
               case response of
                 Left e  -> print e
                 Right r -> mapM_ (\s -> putStrLn $ "* " ++ s) . fromMaybe [] . shouts $ r
               putStrLn ""
  where shouts = mapM (getContent <=< lookupChild "body") <=< lookupChildren "shout" <=< lookupChild "shouts" <=< wrap

share :: APIKey -> SessionKey -> IO ()
share apiKey sessionKey = do response <- Event.share event [Recipient "liblastfm"] (Just $ Message "Just listen!") Nothing apiKey sessionKey
                             case response of
                               Left e  -> print e
                               Right () -> return ()

common :: IO ()
common = do getAttendees
            getInfo
            getShouts

auth :: APIKey -> SessionKey -> IO ()
auth apiKey sessionKey = do attend apiKey sessionKey
                            share apiKey sessionKey
                         -- shout (see User.shout example)
