module Network.Lastfm.Artist
  ( getCorrection, search, share, shout, addTags, removeTag
  ) where

import Control.Exception (throw)

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), APIKey, Artist, Limit, Message, Page, Public, Recipient, SessionKey, Tag)

getCorrection :: Artist -> APIKey -> Lastfm Response
getCorrection artist apiKey = dispatch $ callAPI "artist.getCorrection"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  ]

shout :: Artist -> Message -> APIKey -> SessionKey -> Lastfm ()
shout artist message apiKey sessionKey = dispatch $ callAPI_ "artist.shout"
  [ "artist" ?< artist
  , "message" ?< message
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
share artist recipients message public apiKey sessionKey = dispatch $ callAPI_ "artist.share"
  [ "artist" ?< artist
  , "recipient" ?< recipients
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  , "public" ?< public
  , "message" ?< message
  ]

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

removeTag :: Artist -> Tag -> APIKey -> SessionKey -> Lastfm ()
removeTag artist tag apiKey sessionKey = dispatch $ callAPI_ "artist.removeTag"
  [ "artist" ?< artist
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
