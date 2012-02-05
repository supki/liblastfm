module Network.Lastfm.API.Event
  ( attend, getAttendees, getInfo, getShouts, share, shout
  ) where

import Control.Exception (throw)

import Network.Lastfm.Core
import Network.Lastfm.Types ( (?<), APIKey, Event, Limit, Message, Page
                            , Public, Recipient, SessionKey, Status
                            )

attend :: Event -> Status -> APIKey -> SessionKey -> Lastfm ()
attend event status apiKey sessionKey = dispatch $ callAPI_ "event.attend"
  [ "event" ?< event
  , "status" ?< status
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getAttendees :: Event -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getAttendees event page limit apiKey = dispatch $ callAPI "event.getAttendees"
  [ "event" ?< event
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getInfo :: Event -> APIKey -> Lastfm Response
getInfo event apiKey = dispatch $ callAPI "event.getInfo"
  [ "event" ?< event
  , "api_key" ?< apiKey
  ]

getShouts :: Event -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts event page limit apiKey = dispatch $ callAPI "event.getShouts"
  [ "event" ?< event
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

share :: Event -> Maybe Public -> Maybe Message -> [Recipient] -> APIKey -> SessionKey -> Lastfm ()
share event public message recipients apiKey sessionKey = dispatch go
  where go
          | null recipients        = throw $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = callAPI_ method
            [ "event" ?< event
            , "public" ?< public
            , "message" ?< message
            , "recipient" ?< recipients
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "event.share"

shout :: Event -> Message -> APIKey -> SessionKey -> Lastfm ()
shout event message apiKey sessionKey = dispatch $ callAPI_ "event.shout"
  [ "event" ?< event
  , "message" ?< message
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
