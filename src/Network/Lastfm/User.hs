{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lastfm user API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.User as User
-- @
module Network.Lastfm.User
  ( getArtistTracks, getBannedTracks, getEvents, getFriends
  , getInfo, getLovedTracks, getNeighbours, getNewReleases
  , getPastEvents, getPersonalTags, getPlaylists, getRecentStations
  , getRecentTracks, getRecommendedArtists, getRecommendedEvents
  , getShouts, getTopAlbums, getTopArtists, getTopTags
  , getTopTracks, getWeeklyAlbumChart, getWeeklyArtistChart
  , getWeeklyChartList, getWeeklyTrackChart, shout
  ) where

import Control.Applicative

import Network.Lastfm.Request


-- | Get a list of tracks by a given artist scrobbled by this user
-- , including scrobble time. Can be limited to specific timeranges, defaults to all time.
--
-- Optional: 'startTimestamp', 'page', 'endTimestamp'
--
-- <http://www.last.fm/api/show/user.getArtistTracks>
getArtistTracks :: Request f (User -> Artist -> APIKey -> Ready)
getArtistTracks = api "user.getArtistTracks"
{-# INLINE getArtistTracks #-}


-- | Returns the tracks banned by the user
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getBannedTracks>
getBannedTracks :: Request f (User -> APIKey -> Ready)
getBannedTracks = api "user.getBannedTracks"
{-# INLINE getBannedTracks #-}


-- | Get a list of upcoming events that this user is attending.
-- Easily integratable into calendars, using the ical standard (see 'more formats' section below).
--
-- Optional: 'page', 'festivalsonly', 'limit'
--
-- <http://www.last.fm/api/show/user.getEvents>
getEvents :: Request f (User -> APIKey -> Ready)
getEvents = api "user.getEvents"
{-# INLINE getEvents #-}


-- | Get a list of the user's friends on Last.fm.
--
-- Optional: 'recenttracks', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getFriends>
getFriends :: Request f (User -> APIKey -> Ready)
getFriends = api "user.getFriends"
{-# INLINE getFriends #-}


-- | Get information about a user profile.
--
-- <http://www.last.fm/api/show/user.getInfo>
getInfo :: Request f (User -> APIKey -> Ready)
getInfo = api "user.getInfo"
{-# INLINE getInfo #-}


-- | Get the last 50 tracks loved by a user.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getLovedTracks>
getLovedTracks :: Request f (User -> APIKey -> Ready)
getLovedTracks = api "user.getLovedTracks"
{-# INLINE getLovedTracks #-}


-- | Get a list of a user's neighbours on Last.fm.
--
-- Optional: 'limit'
--
-- <http://www.last.fm/api/show/user.getNeighbours>
getNeighbours :: Request f (User -> APIKey -> Ready)
getNeighbours = api "user.getNeighbours"
{-# INLINE getNeighbours #-}


-- | Gets a list of forthcoming releases based on a user's musical taste.
--
-- Optional: 'userecs'
--
-- <http://www.last.fm/api/show/user.getNewReleases>
getNewReleases :: Request f (User -> APIKey -> Ready)
getNewReleases = api "user.getNewReleases"
{-# INLINE getNewReleases #-}


-- | Get a paginated list of all events a user has attended in the past.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/user.getPastEvents>
getPastEvents :: Request f (User -> APIKey -> Ready)
getPastEvents = api "user.getPastEvents"
{-# INLINE getPastEvents #-}


-- | Get the user's personal tags
--
-- Optional: 'taggingtype', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getPersonalTags>
getPersonalTags :: Request f (User -> Tag -> TaggingType -> APIKey -> Ready)
getPersonalTags = api "user.getPersonalTags"
{-# INLINE getPersonalTags #-}


-- | Get a list of a user's playlists on Last.fm.
--
-- <http://www.last.fm/api/show/user.getPlaylists>
getPlaylists :: Request f (User -> APIKey -> Ready)
getPlaylists = api "user.getPlaylists"
{-# INLINE getPlaylists #-}


-- | Get a list of the recent Stations listened to by this user.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getRecentStations>
getRecentStations :: Request f (User -> APIKey -> SessionKey -> Sign)
getRecentStations = api "user.getRecentStations"
{-# INLINE getRecentStations #-}


-- | Get a list of the recent tracks listened to by this user.
-- Also includes the currently playing track with the nowplaying="true"
-- attribute if the user is currently listening.
--
-- Optional: 'limit', 'page', 'from', 'extended', 'to'
--
-- <http://www.last.fm/api/show/user.getRecentTracks>
getRecentTracks :: Request f (User -> APIKey -> Ready)
getRecentTracks = api "user.getRecentTracks"
{-# INLINE getRecentTracks #-}


-- | Get Last.fm artist recommendations for a user
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/user.getRecommendedArtists>
getRecommendedArtists :: Request f (APIKey -> SessionKey -> Sign)
getRecommendedArtists = api "user.getRecommendedArtists"
{-# INLINE getRecommendedArtists #-}


-- | Get a paginated list of all events recommended to a user by Last.fm, based on their listening profile.
--
-- Optional: 'limit', 'page', 'latitude', 'longitude', 'festivalsonly', 'country'
--
-- <http://www.last.fm/api/show/user.getRecommendedEvents>
getRecommendedEvents :: Request f (APIKey -> SessionKey -> Sign)
getRecommendedEvents = api "user.getRecommendedEvents"
{-# INLINE getRecommendedEvents #-}


-- | Get shouts for this user. Also available as an rss feed.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/user.getShouts>
getShouts :: Request f (User -> APIKey -> Ready)
getShouts = api "user.getShouts"
{-# INLINE getShouts #-}


-- | Get the top albums listened to by a user.
-- You can stipulate a time period. Sends the overall chart by default.
--
-- Optional: 'period', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getTopAlbums>
getTopAlbums :: Request f (User -> APIKey -> Ready)
getTopAlbums = api "user.getTopAlbums"
{-# INLINE getTopAlbums #-}


-- | Get the top artists listened to by a user.
-- You can stipulate a time period. Sends the overall chart by default.
--
-- Optional: 'period', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getTopArtists>
getTopArtists :: Request f (User -> APIKey -> Ready)
getTopArtists = api "user.getTopArtists"
{-# INLINE getTopArtists #-}


-- | Get the top tags used by this user.
--
-- Optional: 'limit'
--
-- <http://www.last.fm/api/show/user.getTopTags>
getTopTags :: Request f (User -> APIKey -> Ready)
getTopTags = api "user.getTopTags"
{-# INLINE getTopTags #-}


-- | Get the top tracks listened to by a user.
-- You can stipulate a time period. Sends the overall chart by default.
--
-- Optional: 'period', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getTopTracks>
getTopTracks :: Request f (User -> APIKey -> Ready)
getTopTracks = api "user.getTopTracks"
{-# INLINE getTopTracks #-}


-- | Get an album chart for a user profile, for a given date range.
-- If no date range is supplied, it will return the most recent album chart for this user.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/user.getWeeklyAlbumChart>
getWeeklyAlbumChart :: Request f (User -> APIKey -> Ready)
getWeeklyAlbumChart = api "user.getWeeklyAlbumChart"
{-# INLINE getWeeklyAlbumChart #-}


-- | Get an artist chart for a user profile, for a given date range.
-- If no date range is supplied, it will return the most recent artist chart for this user.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/user.getWeeklyArtistChart>
getWeeklyArtistChart :: Request f (User -> APIKey -> Ready)
getWeeklyArtistChart = api "user.getWeeklyArtistChart"
{-# INLINE getWeeklyArtistChart #-}


-- | Get a list of available charts for this user, expressed as
-- date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/user.getWeeklyChartList>
getWeeklyChartList :: Request f (User -> APIKey -> Ready)
getWeeklyChartList = api "user.getWeeklyChartList"
{-# INLINE getWeeklyChartList #-}


-- | Get a track chart for a user profile, for a given date range.
-- If no date range is supplied, it will return the most recent track chart for this user.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/user.getWeeklyTrackChart>
getWeeklyTrackChart :: Request f (User -> APIKey -> Ready)
getWeeklyTrackChart = api "user.getWeeklyTrackChart"
{-# INLINE getWeeklyTrackChart #-}


-- | Shout on this user's shoutbox
--
-- <http://www.last.fm/api/show/user.shout>
shout :: Request f (User -> Message -> APIKey -> SessionKey -> Sign)
shout = api "user.shout" <* post
{-# INLINE shout #-}
