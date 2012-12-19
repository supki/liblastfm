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
  ( addTags, getBuyLinks, getInfo, getShouts
  , getTags, getTopTags, removeTag, search, share
  ) where

import Control.Applicative
import Data.Void (Void)

import Network.Lastfm.Request


-- | Tag an album using a list of user supplied tags.
--
-- <http://www.last.fm/api/show/album.addTags>
addTags ∷ Request f RequireSign (Artist → Album → [Tag] → APIKey → SessionKey → Void)
addTags = api "album.addTags" <* post


-- | Get a list of Buy Links for a particular Album. It is
-- required that you supply either the artist and track params or the mbid parameter.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getBuylinks>
getBuyLinks ∷ ArtistAlbumOrMBID t ⇒ Request f Ready (Country → t → APIKey → Void)
getBuyLinks = api "album.getBuyLinks"


-- | Get the metadata for an album on Last.fm using the album name or
-- a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- Optional: 'autocorrect', 'username', 'language'
--
-- <http://www.last.fm/api/show/album.getInfo>
getInfo ∷ ArtistAlbumOrMBID t ⇒ Request f Ready (t → APIKey → Void)
getInfo = api "album.getInfo"


-- | Get shouts for this album.
--
-- Optional: 'autocorrect', 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.getShouts>
getShouts ∷ ArtistAlbumOrMBID t ⇒ Request f Ready (t → APIKey → Void)
getShouts = api "album.getShouts"


-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- Optional: 'autocorrect', 'user'
--
-- <http://www.last.fm/api/show/album.getTags>
getTags ∷ ArtistAlbumOrMBID t ⇒ Request f a (t → APIKey → Void)
getTags = api "album.getTags"


-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getTopTags>
getTopTags ∷ ArtistAlbumOrMBID t ⇒ Request f Ready (t → APIKey → Void)
getTopTags = api "album.getTopTags"


-- | Remove a user's tag from an album.
--
-- <http://www.last.fm/api/show/album.removeTag>
removeTag ∷ Request f RequireSign (Artist → Album → Tag → APIKey → SessionKey → Void)
removeTag = api "album.removeTag" <* post


-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.search>
search ∷ Request f Ready (Album → APIKey → Void)
search = api "album.search"


-- | Share an album with one or more Last.fm users or other friends.
--
-- Optional: 'public', 'message', 'recipient'
--
-- <http://www.last.fm/api/show/album.share>
share ∷ Request f RequireSign (Album → Artist → Recipient → APIKey → SessionKey → Void)
share = api "album.share" <* post
