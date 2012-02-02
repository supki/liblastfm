{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Album
  ( Album(..), Tag(..)
  , addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Control.Exception (throw)
import Data.Maybe (isJust)
import Prelude hiding (either)

import Network.Lastfm.Artist (Artist)
import Network.Lastfm.Auth (APIKey, SessionKey)
import Network.Lastfm.Core

newtype Album = Album String deriving (Show, LastfmValue)
newtype Autocorrect = Autocorrect Bool deriving (Show, LastfmValue)
newtype Country = Country String deriving (Show, LastfmValue)
newtype Language = Language String deriving (Show, LastfmValue)
newtype Limit = Limit Int deriving (Show, LastfmValue)
newtype Mbid = Mbid String deriving (Show, LastfmValue)
newtype Message = Message String deriving (Show, LastfmValue)
newtype Page = Page Int deriving (Show, LastfmValue)
newtype Public = Public Bool deriving (Show, LastfmValue)
newtype Recipient = Recipient String deriving (Show, LastfmValue)
newtype Tag = Tag String deriving (Show, LastfmValue)
newtype Username = Username String deriving (Show, LastfmValue)

addTags :: Artist -> Album -> [Tag] -> APIKey -> SessionKey -> Lastfm ()
addTags artist album tags apiKey sessionKey
  | null tags        = throw $ WrapperCallError "album.addTags" "empty tag list."
  | length tags > 10 = throw $ WrapperCallError "album.addTags" "tag list length has exceeded maximum."
  | otherwise        = dispatch $ callAPI_ "album.addTags"
  [ "artist" ?< artist
  , "album" ?< album
  , "tags" ?< tags
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getBuyLinks :: Maybe (Artist, Album) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Country -> APIKey -> Lastfm Response
getBuyLinks a mbid autocorrect country apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "country" ?< country
  , "api_key" ?< apiKey
  ]
  where method = "album.getBuyLinks"
        parameters = either method a mbid

getInfo :: Maybe (Artist, Album) -> Maybe Mbid -> Maybe Language -> Maybe Autocorrect -> Maybe Username -> APIKey -> Lastfm Response
getInfo a mbid lang autocorrect username apiKey = dispatch $ callAPI method $ parameters ++
  [ "lang" ?< lang
  , "autocorrect" ?< autocorrect
  , "username" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "album.getInfo"
        parameters = either method a mbid

getShouts :: Maybe (Artist, Album) -> Maybe Mbid -> Maybe Limit -> Maybe Autocorrect -> Maybe Page -> APIKey -> Lastfm Response
getShouts a mbid limit autocorrect page apiKey = dispatch $ callAPI method $ parameters ++
  [ "limit" ?< limit
  , "autocorrect" ?< autocorrect
  , "page" ?< page
  , "api_key" ?< apiKey
  ]
  where method = "album.getShouts"
        parameters = either method a mbid

getTags :: Maybe (Artist, Album) -> Maybe Mbid -> Maybe Autocorrect -> Maybe Username -> APIKey -> Lastfm Response
getTags a mbid autocorrect username apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "user" ?< username
  , "api_key" ?< apiKey
  ]
  where method = "album.getTags"
        parameters = either method a mbid

getTopTags :: Maybe (Artist, Album) -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags a mbid autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "album.getTopTags"
        parameters = either method a mbid

removeTag :: Artist -> Album -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist album tag apiKey sessionKey = dispatch $ callAPI_ "album.removeTag"
  [ "artist" ?< artist
  , "album" ?< album
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

search :: Maybe Limit -> Maybe Page -> Album -> APIKey -> Lastfm Response
search limit page album apiKey = dispatch $ callAPI "album.search"
  [ "album" ?< album
  , "api_key" ?< apiKey
  , "limit" ?< limit
  , "page" ?< page
  ]

share :: Artist -> Album -> Maybe Public -> Maybe Message -> [Recipient] -> APIKey -> SessionKey -> Lastfm ()
share artist album public message recipients apiKey sessionKey
  | null recipients        = throw $ WrapperCallError "album.share" "empty recipient list."
  | length recipients > 10 = throw $ WrapperCallError "album.share" "recipient list length has exceeded maximum."
  | otherwise              = dispatch $ callAPI_ "album.share"
    [ "artist" ?< artist
    , "album" ?< album
    , "public" ?< public
    , "message" ?< message
    , "recipient" ?< recipients
    , "api_key" ?< apiKey
    , "sk" ?< sessionKey
    ]

either method a mbid
  | isJust mbid = [ "mbid" ?< mbid ]
  | otherwise   = case a of
                    Just (artist, album) -> [ "artist" ?< artist, "album" ?< album ]
                    Nothing              -> throw $ WrapperCallError method "no mbid nor (artist, album) are specified."
