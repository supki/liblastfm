{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm user API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.User as User
-- @
module Network.Lastfm.User
  ( getArtistTracks, getBannedTracks, getEvents, getFriends, getInfo, getLovedTracks, getNeighbours, getNewReleases, getPastEvents, getPersonalTags, getPlaylists, getRecentStations, getRecentTracks, getRecommendedArtists, getRecommendedEvents, getShouts, getTopAlbums, getTopArtists, getTopTags, getTopTracks, getWeeklyAlbumChart, getWeeklyArtistChart, getWeeklyChartList, getWeeklyTrackChart, shout
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Get a list of tracks by a given artist scrobbled by this user, including scrobble time. Can be limited to specific timeranges, defaults to all time.
--
-- Optional: 'startTimestamp', 'page', 'endTimestamp'
--
-- <http://www.last.fm/api/show/user.getArtistTracks>
getArtistTracks ∷ User → Artist → Request f Ready
getArtistTracks u a = api "user.getArtistTracks" <> user u <> artist a


-- | Returns the tracks banned by the user
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getBannedTracks>
getBannedTracks ∷ User → Request f Ready
getBannedTracks u = api "user.getBannedTracks" <> user u


-- | Get a list of upcoming events that this user is attending. Easily integratable into calendars, using the ical standard (see 'more formats' section below).
--
-- Optional: 'page', 'festivalsonly', 'limit'
--
-- <http://www.last.fm/api/show/user.getEvents>
getEvents ∷ User → Request f Ready
getEvents u = api "user.getEvents" <> user u


-- | Get a list of the user's friends on Last.fm.
--
-- Optional: 'recenttracks', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getFriends>
getFriends ∷ User → Request f Ready
getFriends u = api "user.getFriends" <> user u


-- | Get information about a user profile.
--
-- <http://www.last.fm/api/show/user.getInfo>
getInfo ∷ User → Request f Ready
getInfo u = api "user.getInfo" <> user u


-- | Get the last 50 tracks loved by a user.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getLovedTracks>
getLovedTracks ∷ User → Request f Ready
getLovedTracks u = api "user.getLovedTracks" <> user u


-- | Get a list of a user's neighbours on Last.fm.
--
-- Optional: 'limit'
--
-- <http://www.last.fm/api/show/user.getNeighbours>
getNeighbours ∷ User → Request f Ready
getNeighbours u = api "user.getNeighbours" <> user u


-- | Gets a list of forthcoming releases based on a user's musical taste.
--
-- Optional: 'userecs'
--
-- <http://www.last.fm/api/show/user.getNewReleases>
getNewReleases ∷ User → Request f Ready
getNewReleases u = api "user.getNewReleases" <> user u


-- | Get a paginated list of all events a user has attended in the past.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/user.getPastEvents>
getPastEvents ∷ User → Request f Ready
getPastEvents u = api "user.getPastEvents" <> user u


-- | Get the user's personal tags
--
-- Optional: 'taggingtype', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getPersonalTags>
getPersonalTags ∷ User → Tag → TaggingType → Request f Ready
getPersonalTags u t tt = api "user.getPersonalTags" <> user u <> tag t <> taggingType tt


-- | Get a list of a user's playlists on Last.fm.
--
-- <http://www.last.fm/api/show/user.getPlaylists>
getPlaylists ∷ User → Request f Ready
getPlaylists u = api "user.getPlaylists" <> user u


-- | Get a list of the recent Stations listened to by this user.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getRecentStations>
getRecentStations ∷ User → Request f RequireSign
getRecentStations u = api "user.getRecentStations" <> user u


-- | Get a list of the recent tracks listened to by this user. Also includes the currently playing track with the nowplaying="true" attribute if the user is currently listening.
--
-- Optional: 'limit', 'page', 'from', 'extended', 'to'
--
-- <http://www.last.fm/api/show/user.getRecentTracks>
getRecentTracks ∷ User → Request f Ready
getRecentTracks u = api "user.getRecentTracks" <> user u


-- | Get Last.fm artist recommendations for a user
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/user.getRecommendedArtists>
getRecommendedArtists ∷ Request f RequireSign
getRecommendedArtists = api "user.getRecommendedArtists"


-- | Get a paginated list of all events recommended to a user by Last.fm, based on their listening profile.
--
-- Optional: 'limit', 'page', 'latitude', 'longitude', 'festivalsonly', 'country'
--
-- <http://www.last.fm/api/show/user.getRecommendedEvents>
getRecommendedEvents ∷ Request f RequireSign
getRecommendedEvents = api "user.getRecommendedEvents"


-- | Get shouts for this user. Also available as an rss feed.
--
-- Optional: 'page', 'limit'
--
-- <http://www.last.fm/api/show/user.getShouts>
getShouts ∷ User → Request f Ready
getShouts u = api "user.getShouts" <> user u


-- | Get the top albums listened to by a user. You can stipulate a time period. Sends the overall chart by default.
--
-- Optional: 'period', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getTopAlbums>
getTopAlbums ∷ User → Request f Ready
getTopAlbums u = api "user.getTopAlbums" <> user u


-- | Get the top artists listened to by a user. You can stipulate a time period. Sends the overall chart by default.
--
-- Optional: 'period', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getTopArtists>
getTopArtists ∷ User → Request f Ready
getTopArtists u = api "user.getTopArtists" <> user u


-- | Get the top tags used by this user.
--
-- Optional: 'limit'
--
-- <http://www.last.fm/api/show/user.getTopTags>
getTopTags ∷ User → Request f Ready
getTopTags u = api "user.getTopTags" <> user u


-- | Get the top tracks listened to by a user. You can stipulate a time period. Sends the overall chart by default.
--
-- Optional: 'period', 'limit', 'page'
--
-- <http://www.last.fm/api/show/user.getTopTracks>
getTopTracks ∷ User → Request f Ready
getTopTracks u = api "user.getTopTracks" <> user u


-- | Get an album chart for a user profile, for a given date range. If no date range is supplied, it will return the most recent album chart for this user.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/user.getWeeklyAlbumChart>
getWeeklyAlbumChart ∷ User → Request f Ready
getWeeklyAlbumChart u = api "user.getWeeklyAlbumChart" <> user u


-- | Get an artist chart for a user profile, for a given date range. If no date range is supplied, it will return the most recent artist chart for this user.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/user.getWeeklyArtistChart>
getWeeklyArtistChart ∷ User → Request f Ready
getWeeklyArtistChart u = api "user.getWeeklyArtistChart" <> user u


-- | Get a list of available charts for this user, expressed as date ranges which can be sent to the chart services.
--
-- <http://www.last.fm/api/show/user.getWeeklyChartList>
getWeeklyChartList ∷ User → Request f Ready
getWeeklyChartList u = api "user.getWeeklyChartList" <> user u


-- | Get a track chart for a user profile, for a given date range. If no date range is supplied, it will return the most recent track chart for this user.
--
-- Optional: 'from', 'to'
--
-- <http://www.last.fm/api/show/user.getWeeklyTrackChart>
getWeeklyTrackChart ∷ User → Request f Ready
getWeeklyTrackChart u = api "user.getWeeklyTrackChart" <> user u


-- | Shout on this user's shoutbox
--
-- <http://www.last.fm/api/show/user.shout>
shout ∷ User → Message → Request f RequireSign
shout u m = api "user.shout" <> user u <> message m <> post
