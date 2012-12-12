{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm album API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Album as Album
-- @
module Network.Lastfm.Album
  ( addTags, getBuyLinks, getBuyLinks_mbid, getInfo, getInfo_mbid, getShouts
  , getShouts_mbid, getTags, getTags_mbid
  , getTopTags, getTopTags_mbid, removeTag, search, share
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Tag an album using a list of user supplied tags.
--
-- <http://www.last.fm/api/show/album.addTags>
addTags ∷ Artist → Album → [Tag] → Request RequireSign f
addTags ar al ts = api "album.addTags" <> artist ar <> album al <> tags ts <> post


getBuyLinks ∷ Artist → Album → Country → Request Ready f
getBuyLinks ar al c = api "album.getBuyLinks" <> album al <> artist ar <> country c

-- | Get a list of Buy Links for a particular Album. It is required that you supply either the artist and track params or the mbid parameter.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getBuylinks>
getBuyLinks_mbid ∷ MBID → Country → Request Ready f
getBuyLinks_mbid m c = api "album.getBuyLinks" <> mbid m <> country c


getInfo ∷ Artist → Album → Request Ready f
getInfo ar al = api "album.getInfo" <> album al <> artist ar

-- | Get the metadata for an album on Last.fm using the album name or a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- Optional: 'autocorrect', 'username', 'language'
--
-- <http://www.last.fm/api/show/album.getInfo>
getInfo_mbid ∷ MBID → Request Ready f
getInfo_mbid m = api "album.getInfo" <> mbid m


getShouts ∷ Artist → Album → Request Ready f
getShouts ar al = api "album.getShouts" <> album al <> artist ar

-- | Get shouts for this album.
--
-- Optional: 'autocorrect', 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.getShouts>
getShouts_mbid ∷ MBID → Request Ready f
getShouts_mbid m = api "album.getShouts" <> mbid m


getTags ∷ Artist → Album → Request a f
getTags ar al = api "album.getTags" <> album al <> artist ar

-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- Optional: 'autocorrect', 'user'
--
-- <http://www.last.fm/api/show/album.getTags>
getTags_mbid ∷ MBID → Request a f
getTags_mbid m = api "album.getTags" <> mbid m


getTopTags ∷ Artist → Album → Request a f
getTopTags ar al = api "album.getTopTags" <> album al <> artist ar

-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getTopTags>
getTopTags_mbid ∷ MBID → Request a f
getTopTags_mbid m = api "album.getTopTags" <> mbid m


-- | Remove a user's tag from an album.
--
-- <http://www.last.fm/api/show/album.removeTag>
removeTag ∷ Artist → Album → Tag → Request RequireSign f
removeTag ar al t = api "album.removeTag" <> artist ar <> album al <> tag t <> post


-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.search>
search ∷ Album → Request Ready f
search al = api "album.search" <> album al


-- | Share an album with one or more Last.fm users or other friends.
--
-- Optional: 'public', 'message', 'recipient'
--
-- <http://www.last.fm/api/show/album.share>
share ∷ Album → Artist → Recipient → Request RequireSign f
share al ar r = api "album.share" <> album al <> artist ar <> recipient r <> post
