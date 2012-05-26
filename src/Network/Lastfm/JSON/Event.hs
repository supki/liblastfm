{-# LANGUAGE TemplateHaskell #-}
-- | Event API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Event
  ( attend, getAttendees, getInfo, getShouts, share, shout
  ) where

import Network.Lastfm
import qualified Network.Lastfm.API.Event as API

$(json ["attend", "getAttendees", "getInfo", "getShouts", "share", "shout"])

-- | Set a user's attendance status for an event.
--
-- More: <http://www.last.fm/api/show/event.attend>
attend ∷ Event → Status → APIKey → SessionKey → Secret → Lastfm Response

-- | Get a list of attendees for an event.
--
-- More: <http://www.last.fm/api/show/event.getAttendees>
getAttendees ∷ Event → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the metadata for an event on Last.fm. Includes attendance and lineup information.
--
-- More: <http://www.last.fm/api/show/event.getInfo>
getInfo ∷ Event → APIKey → Lastfm Response

-- | Get shouts for this event.
--
-- More: <http://www.last.fm/api/show/event.getShouts>
getShouts ∷ Event → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Share an event with one or more Last.fm users or other friends.
--
-- More: <http://www.last.fm/api/show/event.share>
share ∷ Event → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response

-- | Shout in this event's shoutbox.
--
-- More: <http://www.last.fm/api/show/event.shout>
shout ∷ Event → Message → APIKey → SessionKey → Secret → Lastfm Response
