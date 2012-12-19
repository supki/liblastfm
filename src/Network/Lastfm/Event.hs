{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm event API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Event as Event
-- @
module Network.Lastfm.Event
  ( attend, getAttendees, getInfo, getShouts, share, shout
  ) where

import Control.Applicative
import Data.Void (Void)

import Network.Lastfm.Request


-- | Set a user's attendance status for an event.
--
-- <http://www.last.fm/api/show/event.attend>
attend ∷ Request f RequireSign (Event → Status → APIKey → SessionKey → Void)
attend = api "event.attend" <* post


-- | Get a list of attendees for an event.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/event.getAttendees>
getAttendees ∷ Request f Ready (Event → APIKey → Void)
getAttendees = api "event.getAttendees"


-- | Get the metadata for an event on Last.fm. Includes attendance and lineup information.
--
-- <http://www.last.fm/api/show/event.getInfo>
getInfo ∷ Request f Ready (Event → APIKey → Void)
getInfo = api "event.getInfo"


-- | Get shouts for this event. Also available as an rss feed.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/event.getShouts>
getShouts ∷ Request f Ready (Event → APIKey → Void)
getShouts = api "event.getShouts"


-- | Share an event with one or more Last.fm users or other friends.
--
-- Optional: 'public', 'message'
--
-- <http://www.last.fm/api/show/event.share>
share ∷ Request f RequireSign (Event → Recipient → APIKey → SessionKey → Void)
share = api "event.share" <* post


-- | Shout in this event's shoutbox
--
-- <http://www.last.fm/api/show/event.shout>
shout ∷ Request f RequireSign (Event → Message → APIKey → SessionKey → Void)
shout = api "event.shout" <* post
