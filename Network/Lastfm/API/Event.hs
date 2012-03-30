-- | Event API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Event
  ( attend, getAttendees, getInfo, getShouts, share, shout
  ) where

import Control.Exception (throw)
import Control.Monad (void)

import Network.Lastfm

-- | Set a user's attendance status for an event.
--
-- More: <http://www.lastfm.ru/api/show/event.attend>
attend :: Event -> Status -> APIKey -> SessionKey -> Secret -> Lastfm ()
attend event status apiKey sessionKey secret = dispatch . void . callAPIsigned secret $
  [ "method" ?< "event.attend"
  , "event" ?< event
  , "status" ?< status
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Get a list of attendees for an event.
--
-- More: <http://www.lastfm.ru/api/show/event.getAttendees>
getAttendees :: Event -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getAttendees event page limit apiKey = dispatch . callAPI $
  [ "method" ?< "event.getAttendees"
  , "event" ?< event
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Get the metadata for an event on Last.fm. Includes attendance and lineup information.
--
-- More: <http://www.lastfm.ru/api/show/event.getInfo>
getInfo :: Event -> APIKey -> Lastfm Response
getInfo event apiKey = dispatch . callAPI $
  [ "method" ?< "event.getInfo"
  , "event" ?< event
  , "api_key" ?< apiKey
  ]

-- | Get shouts for this event.
--
-- More: <http://www.lastfm.ru/api/show/event.getShouts>
getShouts :: Event -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts event page limit apiKey = dispatch . callAPI $
  [ "method" ?< "event.getShouts"
  , "event" ?< event
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Share an event with one or more Last.fm users or other friends.
--
-- More: <http://www.lastfm.ru/api/show/event.share>
share :: Event -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Secret -> Lastfm ()
share event recipients message public apiKey sessionKey secret = dispatch go
  where go
          | null recipients        = throw $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = void $ callAPIsigned secret
            [ "method" ?< method
            , "event" ?< event
            , "public" ?< public
            , "message" ?< message
            , "recipient" ?< recipients
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "event.share"

-- | Shout in this event's shoutbox.
--
-- More: <http://www.lastfm.ru/api/show/event.shout>
shout :: Event -> Message -> APIKey -> SessionKey -> Secret -> Lastfm ()
shout event message apiKey sessionKey secret = dispatch . void . callAPIsigned secret $
  [ "method" ?< "event.shout"
  , "event" ?< event
  , "message" ?< message
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
