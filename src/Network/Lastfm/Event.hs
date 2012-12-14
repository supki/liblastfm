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

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Set a user's attendance status for an event.
--
-- <http://www.last.fm/api/show/event.attend>
attend ∷ Event → Status → Request f RequireSign
attend e s = api "event.attend" <> event e <> status s <> post


-- | Get a list of attendees for an event.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/event.getAttendees>
getAttendees ∷ Event → Request f Ready
getAttendees e = api "event.getAttendees" <> event e


-- | Get the metadata for an event on Last.fm. Includes attendance and lineup information.
--
-- <http://www.last.fm/api/show/event.getInfo>
getInfo ∷ Event → Request f Ready
getInfo e = api "event.getInfo" <> event e


-- | Get shouts for this event. Also available as an rss feed.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/event.getShouts>
getShouts ∷ Event → Request f Ready
getShouts e = api "event.getShouts" <> event e


-- | Share an event with one or more Last.fm users or other friends.
--
-- Optional: 'public', 'message'
--
-- <http://www.last.fm/api/show/event.share>
share ∷ Event → Recipient → Request f RequireSign
share e r = api "event.share" <> event e <> recipient r <> post


-- | Shout in this event's shoutbox
--
-- <http://www.last.fm/api/show/event.shout>
shout ∷ Event → Message → Request f RequireSign
shout e m = api "event.shout" <> event e <> message m <> post
