{-# LANGUAGE TemplateHaskell #-}
-- | Album API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Album as API

$(xml ["addTags", "getBuyLinks", "getInfo", "getShouts", "getTags", "getTopTags", "removeTag", "search", "share"])

-- | Tag an album using a list of user supplied tags.
--
-- More: <http://www.last.fm/api/show/album.addTags>
addTags ∷ (Artist, Album) → [Tag] → APIKey → SessionKey → Secret → Lastfm Response

-- | Get a list of Buy Links for a particular Album. It is required that you supply either the artist and track params or the mbid param.
--
-- More: <http://www.last.fm/api/show/album.getBuylinks>
getBuyLinks ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Country → APIKey → Lastfm Response

-- | Get the metadata for an album on Last.fm using the album name or a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- More: <http://www.last.fm/api/show/album.getInfo>
getInfo ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Language → Maybe Username → APIKey → Lastfm Response

-- | Get shouts for this album.
--
-- More: <http://www.last.fm/api/show/album.getShouts>
getShouts ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- More: <http://www.last.fm/api/show/album.getTags>
getTags ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response

-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- More: <http://www.last.fm/api/show/album.getTopTags>
getTopTags ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → APIKey → Lastfm Response

-- | Remove a user's tag from an album.
--
-- More: <http://www.last.fm/api/show/album.removeTag>
removeTag ∷ Artist → Album → Tag → APIKey → SessionKey → Secret → Lastfm Response

-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- More: <http://www.last.fm/api/show/album.search>
search ∷ Album → Maybe Page → Maybe Limit → APIKey → Lastfm Response

-- | Share an album with one or more Last.fm users or other friends.
--
-- More: <http://www.last.fm/api/show/album.share>
share ∷ Artist → Album → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response
