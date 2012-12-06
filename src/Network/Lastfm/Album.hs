{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Tag an album using a list of user supplied tags.
--
-- <http://www.last.fm/api/show/album.addTags>
addTags ∷ Artist → Album → [Tag] → Request RequireSign f
addTags ar al ts = api "album.addTags" <> artist ar <> album al <> tags ts <> post


-- | Get a list of Buy Links for a particular Album. It is required that you supply either the artist and track params or the mbid parameter.
--
-- Optional: either 'mbid' or both 'artist' and 'album', 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getBuylinks>
getBuyLinks ∷ Country → Request Ready f
getBuyLinks c = api "album.getBuyLinks" <> country c


-- | Get the metadata for an album on Last.fm using the album name or a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- Optional: either 'mbid' or both 'artist' and 'album', 'autocorrect', 'username', 'language'
--
-- <http://www.last.fm/api/show/album.getInfo>
getInfo ∷ Request Ready f
getInfo = api "album.getInfo"


-- | Get shouts for this album.
--
-- Optional: either 'mbid' or both 'artist' and 'album', 'autocorrect', 'limit', 'page'
--
-- <http://www.last.fm/api/show/album.getShouts>
getShouts ∷ Request Ready f
getShouts = api "album.getShouts"


-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- Optional: either 'mbid' or both 'artist' and 'album', 'autocorrect', 'user'
--
-- <http://www.last.fm/api/show/album.getTags>
getTags ∷ Request a f
getTags = api "album.getTags"


-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- Optional: either 'mbid' or both 'artist' and 'album', 'autocorrect'
--
-- <http://www.last.fm/api/show/album.getTopTags>
getTopTags ∷ Request a f
getTopTags = api "album.getTopTags"


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
