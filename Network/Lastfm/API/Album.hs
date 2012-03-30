-- | Album API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Control.Arrow ((|||))
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad (void)
import Network.Lastfm

-- | Tag an album using a list of user supplied tags.
--
-- More: <http://www.lastfm.ru/api/show/album.addTags>
addTags :: (Artist, Album) -> [Tag] -> APIKey -> SessionKey -> Secret -> Lastfm ()
addTags (artist, album) tags apiKey sessionKey secret = runErrorT go
  where go
          | null tags        = throwError $ WrapperCallError method "empty tag list."
          | length tags > 10 = throwError $ WrapperCallError method "tag list length has exceeded maximum."
          | otherwise        = void $ callAPIsigned secret
          [ (#) (Method method)
          , "artist" ?< artist
          , "album" ?< album
          , "tags" ?< tags
          , (#) apiKey
          , (#) sessionKey
          ]
          where method = "album.addTags"

-- | Get a list of Buy Links for a particular Album. It is required that you supply either the artist and track params or the mbid param.
--
-- More: <http://www.lastfm.ru/api/show/album.getBuylinks>
getBuyLinks :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Country -> APIKey -> Lastfm Response
getBuyLinks a autocorrect country apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "album.getBuyLinks")
  , (#) autocorrect
  , "country" ?< country
  , (#) apiKey
  ]

-- | Get the metadata for an album on Last.fm using the album name or a musicbrainz id. See playlist.fetch on how to get the album playlist.
--
-- More: <http://www.lastfm.ru/api/show/album.getInfo>
getInfo :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Maybe Language -> Maybe User -> APIKey -> Lastfm Response
getInfo a autocorrect lang username apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "album.getInfo")
  , (#) autocorrect
  , (#) lang
  , "username" ?< username
  , (#) apiKey
  ]

-- | Get shouts for this album.
--
-- More: <http://www.lastfm.ru/api/show/album.getShouts>
getShouts :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts a autocorrect page limit apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "album.getShouts")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get the tags applied by an individual user to an album on Last.fm.
--
-- More: <http://www.lastfm.ru/api/show/album.getTags>
getTags :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Either User (SessionKey, Secret) -> APIKey -> Lastfm Response
getTags a autocorrect b apiKey = runErrorT $ case b of
  Left user -> callAPI $ target a ++ ["user" ?< user] ++ args
  Right (sessionKey, secret) -> callAPIsigned secret $ target a ++ ["sk" ?< sessionKey] ++ args
  where args =
          [ (#) (Method "album.getTags")
          , (#) autocorrect
          , (#) apiKey
          ]

-- | Get the top tags for an album on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/album.getTopTags>
getTopTags :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a autocorrect apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "album.getTopTags")
  , (#) autocorrect
  , (#) apiKey
  ]

-- | Remove a user's tag from an album.
--
-- More: <http://www.lastfm.ru/api/show/album.removeTag>
removeTag :: Artist -> Album -> Tag -> APIKey -> SessionKey -> Secret -> Lastfm ()
removeTag artist album tag apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ (#) (Method "album.removeTag")
  , "artist" ?< artist
  , "album" ?< album
  , (#) tag
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Search for an album by name. Returns album matches sorted by relevance.
--
-- More: <http://www.lastfm.ru/api/show/album.search>
search :: Album -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search album page limit apiKey = runErrorT . callAPI $
  [ (#) (Method "album.search")
  , "album" ?< album
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Share an album with one or more Last.fm users or other friends.
--
-- More: <http://www.lastfm.ru/api/show/album.share>
share :: Artist -> Album -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Secret -> Lastfm ()
share artist album recipients message public apiKey sessionKey secret = runErrorT go
  where go
          | null recipients        = throwError $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throwError $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = void $ callAPIsigned secret
            [ (#) (Method method)
            , "artist" ?< artist
            , "album" ?< album
            , "public" ?< public
            , "message" ?< message
            , "recipient" ?< recipients
            , (#) apiKey
            , (#) sessionKey
            ]
            where method = "album.share"

target :: Either (Artist, Album) Mbid -> [(String, String)]
target = (\(artist, album) -> ["artist" ?< artist, "album" ?< album]) ||| return . ("mbid" ?<)
