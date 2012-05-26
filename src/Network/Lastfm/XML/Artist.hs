{-# LANGUAGE TemplateHaskell #-}
-- | Artist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Artist
  ( addTags, getCorrection, getEvents, getImages, getInfo
  , getPastEvents, getPodcast, getShouts, getSimilar, getTags, getTopAlbums
  , getTopFans, getTopTags, getTopTracks, removeTag, search, share, shout
  ) where

import Network.Lastfm
import qualified Network.Lastfm.API.Artist as API
  
$(xml ["addTags", "getCorrection", "getEvents", "getImages", "getInfo", "getPastEvents", "getPodcast", "getShouts", "getSimilar", "getTags", "getTopAlbums", "getTopFans", "getTopTags", "getTopTracks", "removeTag", "search", "share", "shout"])

-- | Tag an album using a list of user supplied tags.
--
-- More: <http://www.last.fm/api/show/artist.addTags>
addTags ∷ Artist → [Tag] → APIKey → SessionKey → Secret → Lastfm Response

-- | Use the last.fm corrections data to check whether the supplied artist has a correction to a canonical artist
--
-- More: <http://www.last.fm/api/show/artist.getCorrection>
getCorrection ∷ Artist → APIKey → Lastfm Response

-- | Get a list of upcoming events for this artist.
--
-- More: <http://www.last.fm/api/show/artist.getEvents>
getEvents ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → Maybe FestivalsOnly → APIKey → Lastfm Response

-- | Get Images for this artist in a variety of sizes.
--
-- More: <http://www.last.fm/api/show/artist.getImages>
getImages ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → Maybe Order → APIKey → Lastfm Response

-- | Get the metadata for an artist. Includes biography.
--
-- More: <http://www.last.fm/api/show/artist.getInfo>
getInfo ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Language → Maybe Username → APIKey → Lastfm Response

-- | Get a paginated list of all the events this artist has played at in the past.
--
-- More: <http://www.last.fm/api/show/artist.getPastEvents>
getPastEvents ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get a podcast of free mp3s based on an artist.
--
-- More: <http://www.last.fm/api/show/artist.getPodcast>
getPodcast ∷ Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response

-- | Get shouts for this artist. Also available as an rss feed.
--
-- More: <http://www.last.fm/api/show/artist.getShouts>
getShouts ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get all the artists similar to this artist.
--
-- More: <http://www.last.fm/api/show/artist.getSimilar>
getSimilar ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Limit → APIKey → Lastfm Response

-- | Get the tags applied by an individual user to an artist on Last.fm. If accessed as an authenticated service /and/ you don't supply a user parameter then this service will return tags for the authenticated user.
--
-- More: <http://www.last.fm/api/show/artist.getTags>
getTags ∷ Either Artist Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response

-- | Get the top albums for an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.last.fm/api/show/artist.getTopAlbums>
getTopAlbums ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the top fans for an artist on Last.fm, based on listening data.
--
-- More: <http://www.last.fm/api/show/artist.getTopFans>
getTopFans ∷ Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response

-- | Get the top tags for an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.last.fm/api/show/artist.getTopTags>
getTopTags ∷ Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response

-- | Get the top tracks by an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.last.fm/api/show/artist.getTopTracks>
getTopTracks ∷ Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Remove a user's tag from an artist.
--
-- More: <http://www.last.fm/api/show/artist.removeTag>
removeTag ∷ Artist → Tag → APIKey → SessionKey → Secret → Lastfm Response

-- | Search for an artist by name. Returns artist matches sorted by relevance.
--
-- More: <http://www.last.fm/api/show/artist.search>
search ∷ Artist → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Share an artist with Last.fm users or other friends.
--
-- More: <http://www.last.fm/api/show/artist.share>
share ∷ Artist → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response

-- | Shout in this artist's shoutbox.
--
-- More: <http://www.last.fm/api/show/artist.shout>
shout ∷ Artist → Message → APIKey → SessionKey → Secret → Lastfm Response
