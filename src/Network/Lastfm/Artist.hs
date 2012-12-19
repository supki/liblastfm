{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm artist API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Artist as Artist
-- @
module Network.Lastfm.Artist
  ( addTags, getCorrection, getEvents, getEvents_mbid, getInfo, getInfo_mbid
  , getPastEvents, getPastEvents_mbid, getPodcast, getPodcast_mbid, getShouts
  , getShouts_mbid, getSimilar, getSimilar_mbid, getTags, getTags_mbid
  , getTopAlbums, getTopAlbums_mbid, getTopFans, getTopFans_mbid, getTopTags
  , getTopTags_mbid, getTopTracks, getTopTracks_mbid, removeTag, search, share
  , shout
  ) where

import Control.Applicative
import Data.Void (Void)

import Network.Lastfm.Request


-- | Tag an artist with one or more user supplied tags.
--
-- <http://www.last.fm/api/show/artist.addTags>
addTags ∷ Request f RequireSign (Artist → [Tag] → APIKey → SessionKey → Void)
addTags = api "artist.addTags" <* post


-- | Use the last.fm corrections data to check whether the
-- supplied artist has a correction to a canonical artist
--
-- <http://www.last.fm/api/show/artist.getCorrection>
getCorrection ∷ Request f Ready (Artist → APIKey → Void)
getCorrection = api "artist.getCorrection"


getEvents ∷ Request f Ready (Artist → APIKey → Void)
getEvents = api "artist.getEvents"

-- | Get a list of upcoming events for this artist. Easily
-- integratable into calendars, using the ical standard (see feeds section below).
--
-- Optional: 'autocorrect', 'limit', 'pages', 'festivalsonly'
--
-- <http://www.last.fm/api/show/artist.getEvents>
getEvents_mbid ∷ Request f Ready (MBID → APIKey → Void)
getEvents_mbid = api "artist.getEvents"


getInfo ∷ Request f Ready (Artist → APIKey → Void)
getInfo = api "artist.getInfo"

-- | Get the metadata for an artist. Includes biography.
--
-- Optional: 'language', 'autocorrect', 'username'
--
-- <http://www.last.fm/api/show/artist.getInfo>
getInfo_mbid ∷ Request f Ready (MBID → APIKey → Void)
getInfo_mbid = api "artist.getInfo"


getPastEvents ∷ Request f Ready (Artist → APIKey → Void)
getPastEvents = api "artist.getPastEvents"

-- | Get a paginated list of all the events this artist has played at in the past.
--
-- Optional: 'page', 'autocorrect', 'limit'
--
-- <http://www.last.fm/api/show/artist.getPastEvents>
getPastEvents_mbid ∷ Request f Ready (MBID → APIKey → Void)
getPastEvents_mbid = api "artist.getPastEvents"


getPodcast ∷ Request f Ready (Artist → APIKey → Void)
getPodcast = api "artist.getPodcast"

-- | Get a podcast of free mp3s based on an artist
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getPodcast>
getPodcast_mbid ∷ Request f Ready (MBID → APIKey → Void)
getPodcast_mbid = api "artist.getPodcast"


getShouts ∷ Request f Ready (Artist → APIKey → Void)
getShouts = api "artist.getShouts"

-- | Get shouts for this artist. Also available as an rss feed.
--
-- Optional:'autocorrect', 'limit', 'page'
--
-- <http://www.last.fm/api/show/artist.getShouts>
getShouts_mbid ∷ Request f Ready (MBID → APIKey → Void)
getShouts_mbid = api "artist.getShouts"


getSimilar ∷ Request f Ready (Artist → APIKey → Void)
getSimilar = api "artist.getSimilar"

-- | Get all the artists similar to this artist
--
-- Optional: 'limit', 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getSimilar>
getSimilar_mbid ∷ Request f Ready (MBID → APIKey → Void)
getSimilar_mbid = api "artist.getSimilar"


getTags ∷ Request f a (Artist → APIKey → Void)
getTags = api "artist.getTags"

-- | Get the tags applied by an individual user to an artist on Last.fm.
-- If accessed as an authenticated service /and/ you don't supply a
-- user parameter then this service will return tags for
-- the authenticated user. To retrieve the list of top tags applied
-- to an artist by all users use 'Network.Lastfm.Artist.getTopTags'.
--
-- Optional: 'user', 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getTags>
getTags_mbid ∷ Request f a (MBID → APIKey → Void)
getTags_mbid = api "artist.getTags"


getTopAlbums ∷ Request f Ready (Artist → APIKey → Void)
getTopAlbums = api "artist.getTopAlbums"

-- | Get the top albums for an artist on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect', 'page', 'limit'
--
-- <http://www.last.fm/api/show/artist.getTopAlbums>
getTopAlbums_mbid ∷ Request f Ready (MBID → APIKey → Void)
getTopAlbums_mbid = api "artist.getTopAlbums"


getTopFans ∷ Request f Ready (Artist → APIKey → Void)
getTopFans = api "artist.getTopFans"

-- | Get the top fans for an artist on Last.fm, based on listening data.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getTopFans>
getTopFans_mbid ∷ Request f Ready (MBID → APIKey → Void)
getTopFans_mbid = api "artist.getTopFans"


getTopTags ∷ Request f Ready (Artist → APIKey → Void)
getTopTags = api "artist.getTopTags"

-- | Get the top tags for an artist on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getTopTags>
getTopTags_mbid ∷ Request f Ready (MBID → APIKey → Void)
getTopTags_mbid = api "artist.getTopTags"


getTopTracks ∷ Request f Ready (Artist → APIKey → Void)
getTopTracks = api "artist.getTopTracks"

-- | Get the top tracks by an artist on Last.fm, ordered by popularity
--
-- Optional: 'autocorrect', 'page', 'limit'
--
-- <http://www.last.fm/api/show/artist.getTopTracks>
getTopTracks_mbid ∷ Request f Ready (MBID → APIKey → Void)
getTopTracks_mbid = api "artist.getTopTracks"


-- | Remove a user's tag from an artist.
--
-- <http://www.last.fm/api/show/artist.removeTag>
removeTag ∷ Request f RequireSign (Artist → Tag → APIKey → SessionKey → Void)
removeTag = api "artist.removeTag" <* post


-- | Search for an artist by name. Returns artist matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/artist.search>
search ∷ Request f Ready (Artist → APIKey → Void)
search = api "artist.search"


-- | Share an artist with Last.fm users or other friends.
--
-- Optional: 'message', 'public'
--
-- <http://www.last.fm/api/show/artist.share>
share ∷ Request f RequireSign (Artist → Recipient → APIKey → SessionKey → Void)
share = api "artist.share" <* post


-- | Shout in this artist's shoutbox
--
-- <http://www.last.fm/api/show/artist.shout>
shout ∷ Request f RequireSign (Artist → Message → APIKey → SessionKey → Void)
shout = api "artist.shout" <* post
