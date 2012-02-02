module Network.Lastfm.Artist
  ( addTags, getCorrection, getEvents, getImages, getInfo
  , getPastEvents, getPodcast, getShouts, getSimilar, getTags, getTopAlbums
  , getTopFans, getTopTags, getTopTracks, removeTag, search, share, shout
  ) where

import Control.Exception (throw)
import Data.Maybe (isJust)
import Prelude hiding (either)

import Network.Lastfm.Core
import Network.Lastfm.Types ( (?<), APIKey, Artist, Autocorrect, FestivalsOnly, Language, Limit
                            , Mbid, Message, Order, Page, Public, Recipient, SessionKey, Tag, User
                            )

addTags :: Artist -> [Tag] -> APIKey -> SessionKey -> Lastfm ()
addTags artist tags apiKey sessionKey
  | null tags        = throw $ WrapperCallError "artist.addTags" "empty tag list."
  | length tags > 10 = throw $ WrapperCallError "artist.addTags" "tag list length has exceeded maximum."
  | otherwise        = dispatch $ callAPI_ "artist.addTags"
    [ "artist" ?< artist
    , "tags" ?< tags
    , "api_key" ?< apiKey
    , "sk" ?< sessionKey
    ]

getCorrection :: Artist -> APIKey -> Lastfm Response
getCorrection artist apiKey = dispatch $ callAPI "artist.getCorrection"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  ]

getEvents :: Maybe Artist -> Maybe Mbid -> Maybe Autocorrect -> Maybe Limit -> Maybe Page -> Maybe FestivalsOnly -> APIKey -> Lastfm Response
getEvents artist mbid autocorrect limit page festivalsOnly apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "limit" ?< limit
  , "page" ?< page
  , "festivalsonly" ?< festivalsOnly
  , "api_key" ?< apiKey
  ]
  where method = "artist.getEvents"
        parameters = either method artist mbid

getImages :: Maybe Artist -> Maybe Mbid -> Maybe Page -> Maybe Limit -> Maybe Autocorrect -> Maybe Order -> APIKey -> Lastfm Response
getImages artist mbid page limit autocorrect order apiKey = dispatch $ callAPI method $ parameters ++
  [ "page" ?< page
  , "limit" ?< limit
  , "autocorrect" ?< autocorrect
  , "order" ?< order
  , "api_key" ?< apiKey
  ]
  where method = "artist.getImages"
        parameters = either method artist mbid

getInfo :: Maybe Artist -> Maybe Mbid -> Maybe Language -> Maybe Autocorrect -> Maybe User -> APIKey -> Lastfm Response
getInfo artist mbid language autocorrect user apiKey = dispatch $ callAPI method $ parameters ++
  [ "lang" ?< language
  , "autocorrect" ?< autocorrect
  , "username" ?< user
  , "api_key" ?< apiKey
  ]
  where method = "artist.getInfo"
        parameters = either method artist mbid

getPastEvents :: Maybe Artist -> Maybe Mbid -> Maybe Page -> Maybe Autocorrect -> Maybe Limit -> APIKey -> Lastfm Response
getPastEvents artist mbid page autocorrect limit apiKey = dispatch $ callAPI method $ parameters ++
  [ "page" ?< page
  , "autocorrect" ?< autocorrect
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getPastEvents"
        parameters = either method artist mbid

getPodcast :: Maybe Artist -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getPodcast artist mbid autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getPodcast"
        parameters = either method artist mbid

getShouts :: Maybe Artist -> Maybe Mbid -> Maybe Limit -> Maybe Autocorrect -> Maybe Page -> APIKey -> Lastfm Response
getShouts artist mbid limit autocorrect page apiKey = dispatch $ callAPI method $ parameters ++
  [ "limit" ?< limit
  , "autocorrect" ?< autocorrect
  , "page" ?< page
  , "api_key" ?< apiKey
  ]
  where method = "artist.getShouts"
        parameters = either method artist mbid

getSimilar :: Maybe Artist -> Maybe Mbid -> Maybe Limit -> Maybe Autocorrect -> APIKey -> Lastfm Response
getSimilar artist mbid limit autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "limit" ?< limit
  , "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getSimilar"
        parameters = either method artist mbid

getTags :: Maybe Artist -> Maybe Mbid -> Maybe User -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTags artist mbid user autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "user" ?< user
  , "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTags"
        parameters = either method artist mbid

getTopAlbums :: Maybe Artist -> Maybe Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopAlbums artist mbid autocorrect page limit apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopAlbums"
        parameters = either method artist mbid

getTopFans :: Maybe Artist -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopFans artist mbid autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopFans"
        parameters = either method artist mbid

getTopTags :: Maybe Artist -> Maybe Mbid -> Maybe Autocorrect -> APIKey -> Lastfm Response
getTopTags artist mbid autocorrect apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopTags"
        parameters = either method artist mbid

getTopTracks :: Maybe Artist -> Maybe Mbid -> Maybe Autocorrect -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks artist mbid autocorrect page limit apiKey = dispatch $ callAPI method $ parameters ++
  [ "autocorrect" ?< autocorrect
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
  where method = "artist.getTopTracks"
        parameters = either method artist mbid

removeTag :: Artist -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist tag apiKey sessionKey = dispatch $ callAPI_ "artist.removeTag"
  [ "artist" ?< artist
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

search :: Maybe Limit -> Maybe Page -> Artist -> APIKey -> Lastfm Response
search limit page artist apiKey = dispatch $ callAPI "artist.search"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  , "limit" ?< limit
  , "page" ?< page
  ]

share :: Artist -> [Recipient] -> Maybe Message -> Maybe Public -> APIKey -> SessionKey -> Lastfm ()
share artist recipients message public apiKey sessionKey
  | null recipients        = throw $ WrapperCallError method "empty recipient list."
  | length recipients > 10 = throw $ WrapperCallError method "recipient list length has exceeded maximum."
  | otherwise              = dispatch $ callAPI_ method
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

either :: String -> Maybe Artist -> Maybe Mbid -> [(String, String)]
either method artist mbid
  | isJust mbid = [ "mbid" ?< mbid ]
  | otherwise   = case artist of
                    Just a  -> [ "artist" ?< a ]
                    Nothing -> throw $ WrapperCallError method "no mbid nor artist are specified."
