-- | Artist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Artist
  ( addTags, getCorrection, getEvents, getImages, getInfo
  , getPastEvents, getPodcast, getShouts, getSimilar, getTags, getTopAlbums
  , getTopFans, getTopTags, getTopTracks, removeTag, search, share, shout
  ) where

import Control.Arrow ((|||))
import Control.Monad.Error (runErrorT, throwError)
import Control.Monad (void)
import Network.Lastfm

-- | Tag an album using a list of user supplied tags.
--
-- More: <http://www.lastfm.ru/api/show/artist.addTags>
addTags :: Artist -> [Tag] -> APIKey -> SessionKey -> Secret -> Lastfm ()
addTags artist tags apiKey sessionKey secret = runErrorT go
  where go
          | null tags        = throwError $ WrapperCallError method "empty tag list."
          | length tags > 10 = throwError $ WrapperCallError method "tag list length has exceeded maximum."
          | otherwise        = void $ callAPIsigned secret
            [ (#) (Method method)
            , "artist" ?< artist
            , "tags" ?< tags
            , (#) apiKey
            , (#) sessionKey
            ]
            where method = "artist.addTags"

-- | Use the last.fm corrections data to check whether the supplied artist has a correction to a canonical artist
--
-- More: <http://www.lastfm.ru/api/show/artist.getCorrection>
getCorrection :: Artist -> APIKey -> Lastfm Response
getCorrection artist apiKey = runErrorT . callAPI $
  [ (#) (Method "artist.getCorrection")
  , "artist" ?< artist
  , (#) apiKey
  ]

-- | Get a list of upcoming events for this artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.getEvents>
getEvents :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents a autocorrect page limit festivalsOnly apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getEvents")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , "festivalsonly" ?< festivalsOnly
  , (#) apiKey
  ]

-- | Get Images for this artist in a variety of sizes.
--
-- More: <http://www.lastfm.ru/api/show/artist.getImages>
getImages :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> Maybe Order -> APIKey -> Lastfm Response
getImages a autocorrect page limit order apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getImages")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , "order" ?< order
  , (#) apiKey
  ]

-- | Get the metadata for an artist. Includes biography.
--
-- More: <http://www.lastfm.ru/api/show/artist.getInfo>
getInfo :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Language -> Maybe User -> APIKey -> Lastfm Response
getInfo a autocorrect language user apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getInfo")
  , (#) autocorrect
  , "lang" ?< language
  , "username" ?< user
  , (#) apiKey
  ]

-- | Get a paginated list of all the events this artist has played at in the past.
--
-- More: <http://www.lastfm.ru/api/show/artist.getPastEvents>
getPastEvents :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getPastEvents a autocorrect page limit apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getPastEvents")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get a podcast of free mp3s based on an artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.getPodcast>
getPodcast :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getPodcast a autocorrect apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getPodcast")
  , (#) autocorrect
  , (#) apiKey
  ]

-- | Get shouts for this artist. Also available as an rss feed.
--
-- More: <http://www.lastfm.ru/api/show/artist.getShouts>
getShouts :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts a autocorrect page limit apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getShouts")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get all the artists similar to this artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.getSimilar>
getSimilar :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Limit -> APIKey -> Lastfm Response
getSimilar a autocorrect limit apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getSimilar")
  , (#) autocorrect
  , (#) limit
  , (#) apiKey
  ]

-- | Get the tags applied by an individual user to an artist on Last.fm. If accessed as an authenticated service /and/ you don't supply a user parameter then this service will return tags for the authenticated user.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTags>
getTags :: Either Artist Mbid -> Maybe Autocorrect -> Either User (SessionKey, Secret) -> APIKey -> Lastfm Response
getTags a autocorrect b apiKey = runErrorT $ case b of
  Left user -> callAPI $ target a ++ ["user" ?< user] ++ args
  Right (sessionKey, secret) -> callAPIsigned secret $ target a ++ [(#) sessionKey] ++ args
  where args =
          [ (#) (Method "artist.getTags")
          , (#) autocorrect
          , (#) apiKey
          ]

-- | Get the top albums for an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopAlbums>
getTopAlbums :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopAlbums a autocorrect page limit apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getTopAlbums")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Get the top fans for an artist on Last.fm, based on listening data.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopFans>
getTopFans :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopFans a autocorrect apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getTopFans")
  , (#) autocorrect
  , (#) apiKey
  ]

-- | Get the top tags for an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopTags>
getTopTags :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a autocorrect apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getTopTags")
  , (#) autocorrect
  , (#) apiKey
  ]

-- | Get the top tracks by an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopTracks>
getTopTracks :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks a autocorrect page limit apiKey = runErrorT . callAPI $
  target a ++
  [ (#) (Method "artist.getTopTracks")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

-- | Remove a user's tag from an artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.removeTag>
removeTag :: Artist -> Tag -> APIKey -> SessionKey -> Secret -> Lastfm ()
removeTag artist tag apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ (#) (Method "artist.removeTag")
  , "artist" ?< artist
  , (#) tag
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Search for an artist by name. Returns artist matches sorted by relevance.
--
-- More: <http://www.lastfm.ru/api/show/artist.search>
search :: Artist -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search artist page limit apiKey = runErrorT . callAPI $
  [ (#) (Method "artist.search")
  , "artist" ?< artist
  , (#) apiKey
  , (#) page
  , (#) limit
  ]

-- | Share an artist with Last.fm users or other friends.
--
-- More: <http://www.lastfm.ru/api/show/artist.share>
share :: Artist -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Secret -> Lastfm ()
share artist recipients message public apiKey sessionKey secret = runErrorT go
  where go
          | null recipients        = throwError $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throwError $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = void $ callAPIsigned secret
            [ (#) (Method method)
            , "artist" ?< artist
            , "recipient" ?< recipients
            , (#) apiKey
            , (#) sessionKey
            , "public" ?< public
            , "message" ?< message
            ]
            where method = "artist.share"

-- | Shout in this artist's shoutbox.
--
-- More: <http://www.lastfm.ru/api/show/artist.shout>
shout :: Artist -> Message -> APIKey -> SessionKey -> Secret -> Lastfm ()
shout artist message apiKey sessionKey secret = runErrorT . void . callAPIsigned secret $
  [ (#) (Method "artist.shout")
  , "artist" ?< artist
  , "message" ?< message
  , (#) apiKey
  , (#) sessionKey
  ]

target :: Either Artist Mbid -> [(String, String)]
target = return . ("artist" ?<) ||| return . ("mbid" ?<)
