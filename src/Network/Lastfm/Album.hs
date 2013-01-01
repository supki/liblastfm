{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
  ( ArtistAlbumOrMBID
  , addTags, getBuyLinks, getInfo, getShouts
  , getTags, getTopTags, removeTag, search, share
  ) where

import Control.Applicative

import Network.Lastfm.Request


-- | Unify ('Artist' → 'Album' → …) and ('MBID' → …)
class ArtistAlbumOrMBID a

instance ArtistAlbumOrMBID (MBID → APIKey → Ready)
instance ArtistAlbumOrMBID (Artist → Album → APIKey → Ready)


-- | Tag an album using a list of user supplied tags.
--
-- <http://www.last.fm/api/show/album.addTags>
addTags ∷ Request f Sign (Artist → Album → [Tag] → APIKey → SessionKey → Ready)
addTags = api "album.addTags" <* post
{-# INLINE addTags #-}


-- | Get a list of Buy Links for a particular Album. It is
-- required that you supply either the artist and track params or the mbid parameter.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getBuylinks>
getBuyLinks ∷ ArtistAlbumOrMBID t ⇒ Request f Send (Country → t)
getBuyLinks = api "album.getBuyLinks"
{-# INLINE getBuyLinks #-}


-- | Get the metadata for an album on Last.fm using the album name or
-- a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- Optional: 'autocorrect', 'username', 'language'
--
-- <http://www.last.fm/api/show/album.getInfo>
getInfo ∷ ArtistAlbumOrMBID t ⇒ Request f Send t
getInfo = api "album.getInfo"
{-# INLINE getInfo #-}


-- | Get shouts for this album.
--
-- Optional: 'autocorrect', 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.getShouts>
getShouts ∷ ArtistAlbumOrMBID t ⇒ Request f Send t
getShouts = api "album.getShouts"
{-# INLINE getShouts #-}


-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- Optional: 'autocorrect', 'user'
--
-- <http://www.last.fm/api/show/album.getTags>
getTags ∷ ArtistAlbumOrMBID t ⇒ Request f a t
getTags = api "album.getTags"
{-# INLINE getTags #-}


-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getTopTags>
getTopTags ∷ ArtistAlbumOrMBID t ⇒ Request f Send t
getTopTags = api "album.getTopTags"
{-# INLINE getTopTags #-}


-- | Remove a user's tag from an album.
--
-- <http://www.last.fm/api/show/album.removeTag>
removeTag ∷ Request f Sign (Artist → Album → Tag → APIKey → SessionKey → Ready)
removeTag = api "album.removeTag" <* post
{-# INLINE removeTag #-}


-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.search>
search ∷ Request f Send (Album → APIKey → Ready)
search = api "album.search"
{-# INLINE search #-}


-- | Share an album with one or more Last.fm users or other friends.
--
-- Optional: 'public', 'message', 'recipient'
--
-- <http://www.last.fm/api/show/album.share>
share ∷ Request f Sign (Album → Artist → Recipient → APIKey → SessionKey → Ready)
share = api "album.share" <* post
{-# INLINE share #-}
