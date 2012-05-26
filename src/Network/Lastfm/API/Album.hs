-- | Album API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Control.Arrow ((|||))
import Network.Lastfm

-- | Tag an album using a list of user supplied tags.
--
-- More: <http://www.last.fm/api/show/album.addTags>
addTags ∷ (Artist, Album) → [Tag] → APIKey → SessionKey → Secret → Lastfm Response
addTags (artist, album) tags apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "album.addTags")
  , (#) artist
  , (#) album
  , (#) tags
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Get a list of Buy Links for a particular Album. It is required that you supply either the artist and track params or the mbid param.
--
-- More: <http://www.last.fm/api/show/album.getBuylinks>
getBuyLinks ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Country → APIKey → Lastfm Response
getBuyLinks a autocorrect country apiKey = callAPI XML $
  target a ++
  [ (#) (Method "album.getBuyLinks")
  , (#) autocorrect
  , (#) country
  , (#) apiKey
  ]

-- | Get the metadata for an album on Last.fm using the album name or a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- More: <http://www.last.fm/api/show/album.getInfo>
getInfo ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Language → Maybe Username → APIKey → Lastfm Response
getInfo a autocorrect lang username apiKey = callAPI XML $
  target a ++
  [ (#) (Method "album.getInfo")
  , (#) autocorrect
  , (#) lang
  , (#) username
  , (#) apiKey
  ]

-- | Get shouts for this album.
--
-- More: <http://www.last.fm/api/show/album.getShouts>
getShouts ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getShouts a autocorrect page limit apiKey = callAPI XML $
  target a ++
  [ (#) (Method "album.getShouts")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- More: <http://www.last.fm/api/show/album.getTags>
getTags ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response
getTags a autocorrect b apiKey = case b of
  Left user → callAPI XML $ target a ++ [(#) user] ++ args
  Right (sessionKey, secret) → callAPIsigned XML secret $ target a ++ [(#) sessionKey] ++ args
  where args =
          [ (#) (Method "album.getTags")
          , (#) autocorrect
          , (#) apiKey
          ]

-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- More: <http://www.last.fm/api/show/album.getTopTags>
getTopTags ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → APIKey → Lastfm Response
getTopTags a autocorrect apiKey = callAPI XML $
  target a ++
  [ (#) (Method "album.getTopTags")
  , (#) autocorrect
  , (#) apiKey
  ]

-- | Remove a user's tag from an album.
--
-- More: <http://www.last.fm/api/show/album.removeTag>
removeTag ∷ Artist → Album → Tag → APIKey → SessionKey → Secret → Lastfm Response
removeTag artist album tag apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "album.removeTag")
  , (#) artist
  , (#) album
  , (#) tag
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- More: <http://www.last.fm/api/show/album.search>
search ∷ Album → Maybe Page → Maybe Limit → APIKey → Lastfm Response
search album page limit apiKey = callAPI XML
  [ (#) (Method "album.search")
  , (#) album
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Share an album with one or more Last.fm users or other friends.
--
-- More: <http://www.last.fm/api/show/album.share>
share ∷ Artist → Album → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response
share artist album recipient message public apiKey sessionKey secret = callAPIsigned XML secret
  [ (#) (Method "album.share")
  , (#) artist
  , (#) album
  , (#) public
  , (#) message
  , (#) recipient
  , (#) apiKey
  , (#) sessionKey
  ]

target ∷ Either (Artist, Album) Mbid → [(String, String)]
target = (\(artist, album) → [(#) artist, (#) album]) ||| return . (#)
