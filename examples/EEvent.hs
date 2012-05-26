module EEvent (common, auth) where

import Control.Arrow ((|||))
import Control.Monad ((<=<))

import Network.Lastfm
import qualified Network.Lastfm.XML.Event as Event

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"
event = Event 3142549

attend :: APIKey -> SessionKey -> Secret -> IO ()
attend ak sk s = Event.attend event Maybe ak sk s >>= print ||| const (return ())

getAttendees :: IO ()
getAttendees = parse r f "First 10 attendees"
  where r = Event.getAttendees event Nothing (Just $ Limit 10) apiKey
        f = mapM (content <=< tag "name") <=< tags "user" <=< tag "attendees"

getInfo :: IO ()
getInfo = parse r f "City"
  where r = Event.getInfo event apiKey
        f = fmap return . content <=< tag "city" <=< tag "location" <=< tag "venue" <=< tag "event"

getShouts :: IO ()
getShouts = parse r f "First 8 shouts"
  where r = Event.getShouts event Nothing (Just $ Limit 8) apiKey
        f = mapM (content <=< tag "body") <=< tags "shout" <=< tag "shouts"

share :: APIKey -> SessionKey -> Secret -> IO ()
share ak sk s = Event.share event (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s >>= print ||| const (return ())

common :: IO ()
common = do getAttendees
            getInfo
            getShouts

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth ak sk s = do attend ak sk s
                  share ak sk s
               -- shout (see User.shout example)
