{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | User API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.User
  ( getArtistTracks, getBannedTracks, getEvents, getFriends, getInfo, getLovedTracks
  , getNeighbours, getNewReleases, getPastEvents, getPersonalTags, getPlaylists, getRecentStations
  , getRecentTracks, getRecommendedArtists, getRecommendedEvents, getShouts, getTopAlbums
  , getTopArtists, getTopTags, getTopTracks, getWeeklyAlbumChart, getWeeklyArtistChart
  , getWeeklyChartList, getWeeklyTrackChart, shout
  ) where

#include "user.docs"

import Network.Lastfm.Internal
import Network.Lastfm.JSON (jsonWrapper)
import qualified Network.Lastfm.API.User as API

$(jsonWrapper ["getArtistTracks", "getBannedTracks", "getEvents", "getFriends", "getInfo", "getLovedTracks", "getNeighbours", "getNewReleases", "getPastEvents", "getPersonalTags", "getPlaylists", "getRecentStations", "getRecentTracks", "getRecommendedArtists", "getRecommendedEvents", "getShouts", "getTopAlbums", "getTopArtists", "getTopTags", "getTopTracks", "getWeeklyAlbumChart", "getWeeklyArtistChart", "getWeeklyChartList", "getWeeklyTrackChart", "shout"])

__getArtistTracks__
getArtistTracks ∷ User → Artist → Maybe StartTimestamp → Maybe EndTimestamp → Maybe Page → APIKey → Lastfm Response

__getBannedTracks__
getBannedTracks ∷ User → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getEvents__
getEvents ∷ User → Maybe Page → Maybe Limit → Maybe FestivalsOnly → APIKey → Lastfm Response

__getFriends__
getFriends ∷ User → Maybe RecentTracks → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getInfo__
getInfo ∷ Maybe User → APIKey → Lastfm Response

__getLovedTracks__
getLovedTracks ∷ User → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getNeighbours__
getNeighbours ∷ User → Maybe Limit → APIKey → Lastfm Response

__getNewReleases__
getNewReleases ∷ User → Maybe UseRecs → APIKey → Lastfm Response

__getPastEvents__
getPastEvents ∷ User → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getPersonalTags__
getPersonalTags ∷ User → Tag → TaggingType → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getPlaylists__
getPlaylists ∷ User → APIKey → Lastfm Response

__getRecentStations__
getRecentStations ∷ User → Maybe Page → Maybe Limit → APIKey → SessionKey → Secret → Lastfm Response

__getRecentTracks__
getRecentTracks ∷ User → Maybe Page → Maybe Limit → Maybe From → Maybe To → APIKey → Lastfm Response

__getRecommendedArtists__
getRecommendedArtists ∷ Maybe Page → Maybe Limit → APIKey → SessionKey → Secret → Lastfm Response

__getRecommendedEvents__
getRecommendedEvents ∷ Maybe Page → Maybe Limit → APIKey → SessionKey → Secret → Lastfm Response

__getShouts__
getShouts ∷ User → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopAlbums__
getTopAlbums ∷ User → Maybe Period → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopArtists__
getTopArtists ∷ User → Maybe Period → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTopTags__
getTopTags ∷ User → Maybe Limit → APIKey → Lastfm Response

__getTopTracks__
getTopTracks ∷ User → Maybe Period → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getWeeklyAlbumChart__
getWeeklyAlbumChart ∷ User → Maybe From → Maybe To → APIKey → Lastfm Response

__getWeeklyArtistChart__
getWeeklyArtistChart ∷ User → Maybe From → Maybe To → APIKey → Lastfm Response

__getWeeklyChartList__
getWeeklyChartList ∷ User → APIKey → Lastfm Response

__getWeeklyTrackChart__
getWeeklyTrackChart ∷ User → Maybe From → Maybe To → APIKey → Lastfm Response

__shout__
shout ∷ User → Message → APIKey → SessionKey → Secret → Lastfm Response
