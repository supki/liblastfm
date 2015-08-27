{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Network.Lastfm.Request


-- | Unify ('Artist' -> 'Album' -> …) and ('MBID' -> …)
class ArtistAlbumOrMBID r a | a -> r

instance ArtistAlbumOrMBID r (MBID -> APIKey -> r)
instance ArtistAlbumOrMBID r (Artist -> Album -> APIKey -> r)


-- | Tag an album using a list of user supplied tags.
--
-- <http://www.last.fm/api/show/album.addTags>
addTags :: Request f (Artist -> Album -> [Tag] -> APIKey -> SessionKey -> Sign)
addTags = api "album.addTags" <* post


-- | Get a list of Buy Links for a particular Album. It is
-- required that you supply either the artist and track params or the mbid parameter.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getBuylinks>
getBuyLinks :: ArtistAlbumOrMBID Ready a => Request f (Country -> a)
getBuyLinks = api "album.getBuyLinks"


-- | Get the metadata for an album on Last.fm using the album name or
-- a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- Optional: 'autocorrect', 'username', 'language'
--
-- <http://www.last.fm/api/show/album.getInfo>
getInfo :: ArtistAlbumOrMBID Ready a => Request f a
getInfo = api "album.getInfo"


-- | Get shouts for this album.
--
-- Optional: 'autocorrect', 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.getShouts>
getShouts :: ArtistAlbumOrMBID Ready a => Request f a
getShouts = api "album.getShouts"


-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- Optional: 'autocorrect', 'user'
--
-- <http://www.last.fm/api/show/album.getTags>
getTags :: ArtistAlbumOrMBID r a => Request f a
getTags = api "album.getTags"


-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- Optional: 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getTopTags>
getTopTags :: ArtistAlbumOrMBID Ready a => Request f a
getTopTags = api "album.getTopTags"


-- | Remove a user's tag from an album.
--
-- <http://www.last.fm/api/show/album.removeTag>
removeTag :: Request f (Artist -> Album -> Tag -> APIKey -> SessionKey -> Sign)
removeTag = api "album.removeTag" <* post


-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- Optional: 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.search>
search :: Request f (Album -> APIKey -> Ready)
search = api "album.search"


-- | Share an album with one or more Last.fm users or other friends.
--
-- Optional: 'public', 'message', 'recipient'
--
-- <http://www.last.fm/api/show/album.share>
share :: Request f (Album -> Artist -> Recipient -> APIKey -> SessionKey -> Sign)
share = api "album.share" <* post
