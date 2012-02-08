module Network.Lastfm.API.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Control.Exception (throw)

import Network.Lastfm.Response
import Network.Lastfm.Types ( (?<), Album, APIKey, Artist, Autocorrect, Country, Language, Limit
                            , Mbid, Message, Page, Public, Recipient, SessionKey, Tag, User)

addTags :: (Artist, Album) -> [Tag] -> APIKey -> SessionKey -> Lastfm ()
addTags (artist, album) tags apiKey sessionKey = dispatch go
  where go
          | null tags        = throw $ WrapperCallError method "empty tag list."
          | length tags > 10 = throw $ WrapperCallError method "tag list length has exceeded maximum."
          | otherwise        = callAPI_ method
          [ "artist" ?< artist
          , "album" ?< album
          , "tags" ?< tags
          , "api_key" ?< apiKey
          , "sk" ?< sessionKey
          ]
          where method = "album.addTags"


getBuyLinks :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Country -> APIKey -> Lastfm Response
getBuyLinks a autocorrect country apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "country" ?< country
  , "api_key" ?< apiKey
  ]
  where method = "album.getBuyLinks"
        target = case a of
                   Left (artist, album) -> ["artist" ?< artist, "album" ?< album]
                   Right mbid           -> ["mbid" ?< mbid]

getInfo :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Maybe Language -> Maybe User -> APIKey -> Lastfm Response
getInfo a autocorrect lang username apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "lang" ?< lang
  , "username" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "album.getInfo"
        target = case a of
                   Left (artist, album) -> ["artist" ?< artist, "album" ?< album]
                   Right mbid           -> ["mbid" ?< mbid]

getShouts :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getShouts a autocorrect page limit apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "album.getShouts"
        target = case a of
                   Left (artist, album) -> ["artist" ?< artist, "album" ?< album]
                   Right mbid           -> ["mbid" ?< mbid]

getTags :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> Either User SessionKey -> APIKey -> Lastfm Response
getTags a autocorrect b apiKey = dispatch $ callAPI method $ target ++ auth ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "album.getTags"
        target = case a of
                   Left (artist, album) -> ["artist" ?< artist, "album" ?< album]
                   Right mbid           -> ["mbid" ?< mbid]
        auth = case b of
                 Left user        -> ["user" ?< user]
                 Right sessionKey -> ["sk" ?< sessionKey]

getTopTags :: Either (Artist, Album) Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a autocorrect apiKey = dispatch $ callAPI method $ target ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "album.getTopTags"
        target = case a of
                   Left (artist, album) -> ["artist" ?< artist, "album" ?< album]
                   Right mbid           -> ["mbid" ?< mbid]

removeTag :: Artist -> Album -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist album tag apiKey sessionKey = dispatch $ callAPI_ "album.removeTag"
  [ "artist" ?< artist
  , "album" ?< album
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

search :: Album -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search album page limit apiKey = dispatch $ callAPI "album.search"
  [ "album" ?< album
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

share :: Artist -> Album -> Maybe Public -> Maybe Message -> [Recipient] -> APIKey -> SessionKey -> Lastfm ()
share artist album public message recipients apiKey sessionKey = dispatch go
  where go
          | null recipients        = throw $ WrapperCallError method "empty recipient list."
          | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
          | otherwise              = callAPI_ method
            [ "artist" ?< artist
            , "album" ?< album
            , "public" ?< public
            , "message" ?< message
            , "recipient" ?< recipients
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "album.share"
