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

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Tag an artist with one or more user supplied tags.
--
-- <http://www.last.fm/api/show/artist.addTags>
addTags ∷ Artist → [Tag] → Request f RequireSign
addTags ar ts = api "artist.addTags" <> artist ar <> tags ts <> post


-- | Use the last.fm corrections data to check whether the
-- supplied artist has a correction to a canonical artist
--
-- <http://www.last.fm/api/show/artist.getCorrection>
getCorrection ∷ Artist → Request f Ready
getCorrection ar = api "artist.getCorrection" <> artist ar


getEvents ∷ Artist → Request f Ready
getEvents ar = api "artist.getEvents" <> artist ar

-- | Get a list of upcoming events for this artist. Easily
-- integratable into calendars, using the ical standard (see feeds section below).
--
-- Optional: 'autocorrect', 'limit', 'pages', 'festivalsonly'
--
-- <http://www.last.fm/api/show/artist.getEvents>
getEvents_mbid ∷ MBID → Request f Ready
getEvents_mbid m = api "artist.getEvents" <> mbid m


getInfo ∷ Artist → Request f Ready
getInfo ar = api "artist.getInfo" <> artist ar

-- | Get the metadata for an artist. Includes biography.
--
-- Optional: 'language', 'autocorrect', 'username'
--
-- <http://www.last.fm/api/show/artist.getInfo>
getInfo_mbid ∷ MBID → Request f Ready
getInfo_mbid m = api "artist.getInfo" <> mbid m


getPastEvents ∷ Artist → Request f Ready
getPastEvents ar = api "artist.getPastEvents" <> artist ar

-- | Get a paginated list of all the events this artist has played at in the past.
--
-- Optional: 'page', 'autocorrect', 'limit'
--
-- <http://www.last.fm/api/show/artist.getPastEvents>
getPastEvents_mbid ∷ MBID → Request f Ready
getPastEvents_mbid m = api "artist.getPastEvents" <> mbid m


getPodcast ∷ Artist → Request f Ready
getPodcast ar = api "artist.getPodcast" <> artist ar

-- | Get a podcast of free mp3s based on an artist
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getPodcast>
getPodcast_mbid ∷ MBID → Request f Ready
getPodcast_mbid m = api "artist.getPodcast" <> mbid m


getShouts ∷ Artist → Request f Ready
getShouts ar = api "artist.getShouts" <> artist ar

-- | Get shouts for this artist. Also available as an rss feed.
--
-- Optional:'autocorrect', 'limit', 'page'
--
-- <http://www.last.fm/api/show/artist.getShouts>
getShouts_mbid ∷ MBID → Request f Ready
getShouts_mbid m = api "artist.getShouts" <> mbid m


getSimilar ∷ Artist → Request f Ready
getSimilar ar = api "artist.getSimilar" <> artist ar

-- | Get all the artists similar to this artist
--
-- Optional: 'limit', 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getSimilar>
getSimilar_mbid ∷ MBID → Request f Ready
getSimilar_mbid m = api "artist.getSimilar" <> mbid m


getTags ∷ Artist → Request f a
getTags ar = api "artist.getTags" <> artist ar

-- | Get the tags applied by an individual user to an artist on Last.fm.
-- If accessed as an authenticated service /and/ you don't supply a
-- user parameter then this service will return tags for
-- the authenticated user. To retrieve the list of top tags applied
-- to an artist by all users use 'Network.Lastfm.Artist.getTopTags'.
--
-- Optional: 'user', 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getTags>
getTags_mbid ∷ MBID → Request f a
getTags_mbid m = api "artist.getTags" <> mbid m


getTopAlbums ∷ Artist → Request f Ready
getTopAlbums ar = api "artist.getTopAlbums" <> artist ar

-- | Get the top albums for an artist on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect', 'page', 'limit'
--
-- <http://www.last.fm/api/show/artist.getTopAlbums>
getTopAlbums_mbid ∷ MBID → Request f Ready
getTopAlbums_mbid m = api "artist.getTopAlbums" <> mbid m


getTopFans ∷ Artist → Request f Ready
getTopFans ar = api "artist.getTopFans" <> artist ar

-- | Get the top fans for an artist on Last.fm, based on listening data.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getTopFans>
getTopFans_mbid ∷ MBID → Request f Ready
getTopFans_mbid m = api "artist.getTopFans" <> mbid m


getTopTags ∷ Artist → Request f Ready
getTopTags ar = api "artist.getTopTags" <> artist ar

-- | Get the top tags for an artist on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/artist.getTopTags>
getTopTags_mbid ∷ MBID → Request f Ready
getTopTags_mbid m = api "artist.getTopTags" <> mbid m


getTopTracks ∷ Artist → Request f Ready
getTopTracks ar = api "artist.getTopTracks" <> artist ar

-- | Get the top tracks by an artist on Last.fm, ordered by popularity
--
-- Optional: 'autocorrect', 'page', 'limit'
--
-- <http://www.last.fm/api/show/artist.getTopTracks>
getTopTracks_mbid ∷ MBID → Request f Ready
getTopTracks_mbid m = api "artist.getTopTracks" <> mbid m


-- | Remove a user's tag from an artist.
--
-- <http://www.last.fm/api/show/artist.removeTag>
removeTag ∷ Artist → Tag → Request f RequireSign
removeTag ar t = api "artist.removeTag" <> artist ar <> tag t <> post


-- | Search for an artist by name. Returns artist matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/artist.search>
search ∷ Artist → Request f Ready
search ar = api "artist.search" <> artist ar


-- | Share an artist with Last.fm users or other friends.
--
-- Optional: 'message', 'public'
--
-- <http://www.last.fm/api/show/artist.share>
share ∷ Artist → Recipient → Request f RequireSign
share ar r = api "artist.share" <> artist ar <> recipient r <> post


-- | Shout in this artist's shoutbox
--
-- <http://www.last.fm/api/show/artist.shout>
shout ∷ Artist → Message → Request f RequireSign
shout ar m = api "artist.shout" <> artist ar <> message m <> post
