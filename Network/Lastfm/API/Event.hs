-- | Event API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Event
  ( attend, getAttendees, getInfo, getShouts, share, shout
  ) where

import Network.Lastfm

-- | Set a user's attendance status for an event.
--
-- More: <http://www.last.fm/api/show/event.attend>
attend :: Event -> Status -> APIKey -> SessionKey -> Secret -> Lastfm Response
attend event status apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "event.attend")
  , (#) event
  , (#) status
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Get a list of attendees for an event.
--
-- More: <http://www.last.fm/api/show/event.getAttendees>
getAttendees :: Event -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getAttendees event page limit apiKey = callAPI XML
  [ (#) (Method "event.getAttendees")
  , (#) event
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get the metadata for an event on Last.fm. Includes attendance and lineup information.
--
-- More: <http://www.last.fm/api/show/event.getInfo>
getInfo :: Event -> APIKey -> Lastfm Response
getInfo event apiKey = callAPI XML
  [ (#) (Method "event.getInfo")
  , (#) event
  , (#) apiKey
  ]

-- | Get shouts for this event.
--
-- More: <http://www.last.fm/api/show/event.getShouts>
getShouts :: Event -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts event page limit apiKey = callAPI XML
  [ (#) (Method "event.getShouts")
  , (#) event
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Share an event with one or more Last.fm users or other friends.
--
-- More: <http://www.last.fm/api/show/event.share>
share :: Event -> Recipient -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Secret -> Lastfm Response
share event recipient message public apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "event.share")
  , (#) event
  , (#) public
  , (#) message
  , (#) recipient
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Shout in this event's shoutbox.
--
-- More: <http://www.last.fm/api/show/event.shout>
shout :: Event -> Message -> APIKey -> SessionKey -> Secret -> Lastfm Response
shout event message apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "event.shout")
  , (#) event
  , (#) message
  , (#) apiKey
  , (#) sessionKey
  ]
