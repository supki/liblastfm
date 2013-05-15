{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Network.Lastfm.Request


-- | Set a user's attendance status for an event.
--
-- <http://www.last.fm/api/show/event.attend>
attend :: Request f Sign (Event -> Status -> APIKey -> SessionKey -> Ready)
attend = api "event.attend" <* post
{-# INLINE attend #-}


-- | Get a list of attendees for an event.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/event.getAttendees>
getAttendees :: Request f Send (Event -> APIKey -> Ready)
getAttendees = api "event.getAttendees"
{-# INLINE getAttendees #-}


-- | Get the metadata for an event on Last.fm. Includes attendance and lineup information.
--
-- <http://www.last.fm/api/show/event.getInfo>
getInfo :: Request f Send (Event -> APIKey -> Ready)
getInfo = api "event.getInfo"
{-# INLINE getInfo #-}


-- | Get shouts for this event. Also available as an rss feed.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/event.getShouts>
getShouts :: Request f Send (Event -> APIKey -> Ready)
getShouts = api "event.getShouts"
{-# INLINE getShouts #-}


-- | Share an event with one or more Last.fm users or other friends.
--
-- Optional: 'public', 'message'
--
-- <http://www.last.fm/api/show/event.share>
share :: Request f Sign (Event -> Recipient -> APIKey -> SessionKey -> Ready)
share = api "event.share" <* post
{-# INLINE share #-}


-- | Shout in this event's shoutbox
--
-- <http://www.last.fm/api/show/event.shout>
shout :: Request f Sign (Event -> Message -> APIKey -> SessionKey -> Ready)
shout = api "event.shout" <* post
{-# INLINE shout #-}
