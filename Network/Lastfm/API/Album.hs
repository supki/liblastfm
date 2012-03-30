-- | Album API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Control.Arrow ((|||))
import Control.Exception (throw)
import Control.Monad (void)

import Network.Lastfm

-- | Tag an album using a list of user supplied tags.
--
-- More: <http://www.lastfm.ru/api/show/album.addTags>
addTags :: (Artist, Album) -> [Tag] -> APIKey -> SessionKey -> Secret -> Lastfm ()
addTags (artist, album) tags apiKey sessionKey secret = dispatch go
  where go
          | null tags        = throw $ WrapperCallError method "empty tag list."
          | length tags > 10 = throw $ WrapperCallError method "tag list length has exceeded maximum."
          | otherwise        = void $ callAPIsigned secret
          [ "method" ?< method
          , "artist" ?< artist
          , "album" ?< album
          , "tags" ?< tags
          , "api_key" ?< apiKey
          , "sk" ?< sessionKey
          ]
          where method = "album.addTags"

-- | Get a list of Buy Links for a particular Album. It is required that you supply either the artist and track params or the mbid param.
--
-- More: <http://www.lastfm.ru/api/show/album.getBuylinks>
getBuyLinks :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Country -> APIKey -> Lastfm Response
getBuyLinks a autocorrect country apiKey = dispatch . callAPI $
  target a ++
  [ "method" ?< "album.getBuyLinks"
  , "autocorrect" ?< autocorrect
  , "country" ?< country
  , "api_key" ?< apiKey
  ]

-- | Get the metadata for an album on Last.fm using the album name or a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- More: <http://www.lastfm.ru/api/show/album.getInfo>
getInfo :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Maybe Language -> Maybe User -> APIKey -> Lastfm Response
getInfo a autocorrect lang username apiKey = dispatch . callAPI $
  target a ++
  [ "method" ?< "album.getInfo"
  , "autocorrect" ?< autocorrect
  , "lang" ?< lang
  , "username" ?< username
  , "api_key" ?< apiKey
  ]

-- | Get shouts for this album.
--
-- More: <http://www.lastfm.ru/api/show/album.getShouts>
getShouts :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts a autocorrect page limit apiKey = dispatch . callAPI $
  target a ++
  [ "method" ?< "album.getShouts"
  , "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- More: <http://www.lastfm.ru/api/show/album.getTags>
getTags :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Either User (SessionKey, Secret) -> APIKey -> Lastfm Response
getTags a autocorrect b apiKey = dispatch $ case b of
  Left user -> callAPI $ target a ++ ["user" ?< user] ++ args
  Right (sessionKey, secret) -> callAPIsigned secret $ target a ++ ["sk" ?< sessionKey] ++ args
  where args =
          [ "method" ?< "album.getTags"
          , "autocorrect" ?< autocorrect
          , "api_key" ?< apiKey
          ]

-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/album.getTopTags>
getTopTags :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a autocorrect apiKey = dispatch . callAPI $
  target a ++
  [ "method" ?< "album.getTopTags"
  , "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]

-- | Remove a user's tag from an album.
--
-- More: <http://www.lastfm.ru/api/show/album.removeTag>
removeTag :: Artist -> Album -> Tag -> APIKey -> SessionKey -> Secret -> Lastfm ()
removeTag artist album tag apiKey sessionKey secret = dispatch . void . callAPIsigned secret $
  [ "method" ?< "album.removeTag"
  , "artist" ?< artist
  , "album" ?< album
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- More: <http://www.lastfm.ru/api/show/album.search>
search :: Album -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search album page limit apiKey = dispatch . callAPI $
  [ "method" ?< "album.search"
  , "album" ?< album
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

-- | Share an album with one or more Last.fm users or other friends.
--
-- More: <http://www.lastfm.ru/api/show/album.share>
share :: Artist -> Album -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Secret -> Lastfm ()
share artist album recipients message public apiKey sessionKey secret = dispatch go
  where go
          | null recipients        = throw $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = void $ callAPIsigned secret
            [ "method" ?< method
            , "artist" ?< artist
            , "album" ?< album
            , "public" ?< public
            , "message" ?< message
            , "recipient" ?< recipients
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "album.share"

target :: Either (Artist, Album) Mbid -> [(String, String)]
target = (\(artist, album) -> ["artist" ?< artist, "album" ?< album]) ||| return . ("mbid" ?<)
