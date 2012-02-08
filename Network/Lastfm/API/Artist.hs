module Network.Lastfm.API.Artist
  ( addTags, getCorrection, getEvents, getImages, getInfo
  , getPastEvents, getPodcast, getShouts, getSimilar, getTags, getTopAlbums
  , getTopFans, getTopTags, getTopTracks, removeTag, search, share, shout
  ) where

import Control.Exception (throw)

import Network.Lastfm.Response
import Network.Lastfm.Types ( (?<), APIKey, Artist, Autocorrect, FestivalsOnly, Language, Limit
                            , Mbid, Message, Order, Page, Public, Recipient, SessionKey, Tag, User
                            )

addTags :: Artist -> [Tag] -> APIKey -> SessionKey -> Lastfm ()
addTags artist tags apiKey sessionKey = dispatch go
  where go
          | null tags        = throw $ WrapperCallError method "empty tag list."
          | length tags > 10 = throw $ WrapperCallError method "tag list length has exceeded maximum."
          | otherwise        = callAPI_ method
            [ "artist" ?< artist
            , "tags" ?< tags
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "artist.addTags"

getCorrection :: Artist -> APIKey -> Lastfm Response
getCorrection artist apiKey = dispatch $ callAPI "artist.getCorrection"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  ]

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

getPodcast :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getPodcast a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getPodcast"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

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

getTopFans :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopFans a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopFans"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

getTopTags :: Either Artist Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopTags"
        target = case a of
                   Left artist -> ["artist" ?< artist]
                   Right mbid  -> ["mbid" ?< mbid]

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

removeTag :: Artist -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist tag apiKey sessionKey = dispatch $ callAPI_ "artist.removeTag"
  [ "artist" ?< artist
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

search :: Artist -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search artist page limit apiKey = dispatch $ callAPI "artist.search"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  , "page" ?< page
  , "limit" ?< limit
  ]

share :: Artist -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Lastfm ()
share artist recipients message public apiKey sessionKey = dispatch go
  where go
          | null recipients        = throw $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = callAPI_ method
            [ "artist" ?< artist
            , "recipient" ?< recipients
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            , "public" ?< public
            , "message" ?< message
            ]
            where method = "artist.share"

shout :: Artist -> Message -> APIKey -> SessionKey -> Lastfm ()
shout artist message apiKey sessionKey = dispatch $ callAPI_ "artist.shout"
  [ "artist" ?< artist
  , "message" ?< message
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
