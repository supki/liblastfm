module Network.Lastfm.API.Radio
  ( getPlaylist, search, tune
  ) where

import Network.Lastfm.Internal

getPlaylist ∷ Format
            → Maybe Discovery
            → Maybe RTP
            → Maybe BuyLinks
            → Multiplier
            → Bitrate
            → APIKey
            → SessionKey
            → Secret
            → Lastfm Response
getPlaylist t discovery rtp buylinks multiplier bitrate apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "radio.getPlaylist")
  , (#) discovery
  , (#) rtp
  , (#) buylinks
  , (#) multiplier
  , (#) bitrate
  , (#) apiKey
  , (#) sessionKey
  ]

search ∷ Format → Name → APIKey → Lastfm Response
search t name apiKey = callAPI t
  [ (#) (Method "radio.search")
  , (#) name
  , (#) apiKey
  ]

tune ∷ Format → Maybe Language → Station → APIKey → SessionKey → Secret → Lastfm Response
tune t language station apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "radio.tune")
  , (#) language
  , (#) station
  , (#) apiKey
  , (#) sessionKey
  ]
