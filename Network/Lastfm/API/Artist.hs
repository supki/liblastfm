-- | Artist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Artist
  ( addTags, getCorrection, getEvents, getImages, getInfo
  , getPastEvents, getPodcast, getShouts, getSimilar, getTags, getTopAlbums
  , getTopFans, getTopTags, getTopTracks, removeTag, search, share, shout
  ) where

import Control.Exception (throw)
import Control.Monad (void)

import Network.Lastfm ( Lastfm, Response, LastfmError (WrapperCallError), callAPI, dispatch
                      , (?<), APIKey, Artist, Autocorrect, FestivalsOnly, Language, Limit
                      , Mbid, Message, Order, Page, Public, Recipient, SessionKey, Tag, User
                      )

-- | Tag an album using a list of user supplied tags.
--
-- More: <http://www.lastfm.ru/api/show/artist.addTags>
addTags :: Artist -> [Tag] -> APIKey -> SessionKey -> Lastfm ()
addTags artist tags apiKey sessionKey = dispatch go
  where go
          | null tags        = throw $ WrapperCallError method "empty tag list."
          | length tags > 10 = throw $ WrapperCallError method "tag list length has exceeded maximum."
          | otherwise        = void $ callAPI method
            [ "artist" ?< artist
            , "tags" ?< tags
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "artist.addTags"

-- | Use the last.fm corrections data to check whether the supplied artist has a correction to a canonical artist
--
-- More: <http://www.lastfm.ru/api/show/artist.getCorrection>
getCorrection :: Artist -> APIKey -> Lastfm Response
getCorrection artist apiKey = dispatch $ callAPI "artist.getCorrection"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  ]

-- | Get a list of upcoming events for this artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.getEvents>
getEvents :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents a autocorrect page limit festivalsOnly apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "festivalsonly" ?< festivalsOnly
  , "api_key" ?< apiKey
  ]
  where method = "artist.getEvents"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get Images for this artist in a variety of sizes.
--
-- More: <http://www.lastfm.ru/api/show/artist.getImages>
getImages :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> Maybe Order -> APIKey -> Lastfm Response
getImages a autocorrect page limit order apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "order" ?< order
  , "api_key" ?< apiKey
  ]
  where method = "artist.getImages"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get the metadata for an artist. Includes biography.
--
-- More: <http://www.lastfm.ru/api/show/artist.getInfo>
getInfo :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Language -> Maybe User -> APIKey -> Lastfm Response
getInfo a autocorrect language user apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "lang" ?< language
  , "username" ?< user
  , "api_key" ?< apiKey
  ]
  where method = "artist.getInfo"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get a paginated list of all the events this artist has played at in the past.
--
-- More: <http://www.lastfm.ru/api/show/artist.getPastEvents>
getPastEvents :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getPastEvents a autocorrect page limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getPastEvents"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get a podcast of free mp3s based on an artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.getPodcast>
getPodcast :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getPodcast a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getPodcast"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get shouts for this artist. Also available as an rss feed.
--
-- More: <http://www.lastfm.ru/api/show/artist.getShouts>
getShouts :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts a autocorrect page limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getShouts"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get all the artists similar to this artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.getSimilar>
getSimilar :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Limit -> APIKey -> Lastfm Response
getSimilar a autocorrect limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getSimilar"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get the tags applied by an individual user to an artist on Last.fm. If accessed as an authenticated service /and/ you don't supply a user parameter then this service will return tags for the authenticated user.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTags>
getTags :: Either Artist Mbid -> Maybe Autocorrect -> Either User SessionKey -> APIKey -> Lastfm Response
getTags a autocorrect b apiKey = dispatch $ callAPI method $ target ++ auth ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTags"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]
        auth = case b of
                 Left user        -> ["user" ?< user]
                 Right sessionKey -> ["sk" ?< sessionKey]

-- | Get the top albums for an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopAlbums>
getTopAlbums :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopAlbums a autocorrect page limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopAlbums"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get the top fans for an artist on Last.fm, based on listening data.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopFans>
getTopFans :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopFans a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopFans"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get the top tags for an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopTags>
getTopTags :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopTags"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Get the top tracks by an artist on Last.fm, ordered by popularity.
--
-- More: <http://www.lastfm.ru/api/show/artist.getTopTracks>
getTopTracks :: Either Artist Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks a autocorrect page limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopTracks"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

-- | Remove a user's tag from an artist.
--
-- More: <http://www.lastfm.ru/api/show/artist.removeTag>
removeTag :: Artist -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist tag apiKey sessionKey = dispatch $ void $ callAPI "artist.removeTag"
  [ "artist" ?< artist
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

-- | Search for an artist by name. Returns artist matches sorted by relevance.
--
-- More: <http://www.lastfm.ru/api/show/artist.search>
search :: Artist -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search artist page limit apiKey = dispatch $ callAPI "artist.search"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  , "page" ?< page
  , "limit" ?< limit
  ]

-- | Share an artist with Last.fm users or other friends.
--
-- More: <http://www.lastfm.ru/api/show/artist.share>
share :: Artist -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Lastfm ()
share artist recipients message public apiKey sessionKey = dispatch go
  where go
          | null recipients        = throw $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = void $ callAPI method
            [ "artist" ?< artist
            , "recipient" ?< recipients
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            , "public" ?< public
            , "message" ?< message
            ]
            where method = "artist.share"

-- | Shout in this artist's shoutbox.
--
-- More: <http://www.lastfm.ru/api/show/artist.shout>
shout :: Artist -> Message -> APIKey -> SessionKey -> Lastfm ()
shout artist message apiKey sessionKey = dispatch $ void $ callAPI "artist.shout"
  [ "artist" ?< artist
  , "message" ?< message
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
