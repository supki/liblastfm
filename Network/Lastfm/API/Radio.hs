-- | Radio API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Radio
  ( getPlaylist, search, tune
  ) where

import Control.Monad.Error (runErrorT)
import Network.Lastfm

-- | Fetch new radio content periodically in an XSPF format.
--
-- More: <http://www.lastfm.ru/api/show/radio.getPlaylist>
getPlaylist :: Maybe Discovery
            -> Maybe RTP
            -> Maybe BuyLinks
            -> Multiplier
            -> Bitrate
            -> APIKey
            -> SessionKey
            -> Secret
            -> Lastfm Response
getPlaylist discovery rtp buylinks multiplier bitrate apiKey sessionKey secret = runErrorT . callAPIsigned secret $
  [ (#) (Method "radio.getPlaylist")
  , (#) discovery
  , (#) rtp
  , (#) buylinks
  , (#) multiplier
  , (#) bitrate
  , (#) apiKey
  , (#) sessionKey
  ]

-- | Resolve the name of a resource into a station depending on which resource it is most likely to represent.
--
-- More: <http://www.lastfm.ru/api/show/radio.search>
search :: Name -> APIKey -> Lastfm Response
search name apiKey = runErrorT . callAPI $
  [ (#) (Method "radio.search")
  , (#) name
  , (#) apiKey
  ]

-- | Tune in to a Last.fm radio station.
--
-- More: <http://www.lastfm.ru/api/show/radio.tune>
tune :: Maybe Language -> Station -> APIKey -> SessionKey -> Secret -> Lastfm Response
tune language station apiKey sessionKey secret = runErrorT . callAPIsigned secret $
  [ (#) (Method "radio.tune")
  , (#) language
  , (#) station
  , (#) apiKey
  , (#) sessionKey
  ]
