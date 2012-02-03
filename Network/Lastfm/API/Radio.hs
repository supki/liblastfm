module Network.Lastfm.API.Radio
  ( getPlaylist, search, tune
  ) where

import Control.Exception (throw)

import Network.Lastfm.Core
import Network.Lastfm.Types ( (?<), APIKey, Bitrate, BuyLinks, Discovery, Language
                            , Multiplier, Name, RTP, Station, SessionKey, unpack
                            )

getPlaylist :: Maybe Discovery
            -> Maybe RTP
            -> Maybe BuyLinks
            -> Maybe Multiplier
            -> Maybe Bitrate
            -> APIKey
            -> SessionKey
            -> Lastfm Response
getPlaylist discovery rtp buylinks multiplier bitrate apiKey sessionKey = dispatch go
  where go
          | unpack multiplier /= "1.0" && unpack multiplier /= "2.0" = throw $ WrapperCallError method "unsupported multiplier."
          | unpack bitrate /= "64" && unpack bitrate /= "128" = throw $ WrapperCallError method "unsupported bitrate."
          | otherwise = callAPI method
            [ "discovery" ?< discovery
            , "rtp" ?< rtp
            , "buylinks" ?< buylinks
            , "speed_multiplier" ?< multiplier
            , "bitrate" ?< bitrate
            , "api_key" ?< apiKey
            , "sk" ?< sessionKey
            ]
            where method = "radio.getPlaylist"

search :: Name -> APIKey -> Lastfm Response
search name apiKey = dispatch $ callAPI "radio.search"
  [ "name" ?< name
  , "api_key" ?< apiKey
  ]

tune :: Language -> Station -> APIKey -> SessionKey -> Lastfm Response
tune language station apiKey sessionKey = dispatch $ callAPI "radio.tune"
  [ "lang" ?< language
  , "station" ?< station
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
