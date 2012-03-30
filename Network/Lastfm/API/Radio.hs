-- | Radio API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.API.Radio
  ( getPlaylist, search, tune
  ) where

import Control.Monad.Error (runErrorT, throwError)
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
getPlaylist discovery rtp buylinks multiplier bitrate apiKey sessionKey secret = runErrorT go
  where go
          | value multiplier /= "1.0" && value multiplier /= "2.0" = throwError $ WrapperCallError method "unsupported multiplier."
          | value bitrate /= "64" && value bitrate /= "128" = throwError $ WrapperCallError method "unsupported bitrate."
          | otherwise = callAPIsigned secret
            [ (#) (Method method)
            , "discovery" ?< discovery
            , "rtp" ?< rtp
            , "buylinks" ?< buylinks
            , "speed_multiplier" ?< multiplier
            , "bitrate" ?< bitrate
            , (#) apiKey
            , (#) sessionKey
            ]
            where method = "radio.getPlaylist"

-- | Resolve the name of a resource into a station depending on which resource it is most likely to represent.
--
-- More: <http://www.lastfm.ru/api/show/radio.search>
search :: Name -> APIKey -> Lastfm Response
search name apiKey = runErrorT . callAPI $
  [ (#) (Method "radio.search")
  , "name" ?< name
  , (#) apiKey
  ]

-- | Tune in to a Last.fm radio station.
--
-- More: <http://www.lastfm.ru/api/show/radio.tune>
tune :: Maybe Language -> Station -> APIKey -> SessionKey -> Secret -> Lastfm Response
tune language station apiKey sessionKey secret = runErrorT . callAPIsigned secret $
  [ (#) (Method "radio.tune")
  , (#) language
  , "station" ?< station
  , (#) apiKey
  , (#) sessionKey
  ]
