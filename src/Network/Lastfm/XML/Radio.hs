{-# LANGUAGE TemplateHaskell #-}
-- | Radio API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Radio
  ( getPlaylist, search, tune
  ) where

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Radio as API

$(xml ["getPlaylist", "search", "tune"])

-- | Fetch new radio content periodically in an XSPF format.
--
-- More: <http://www.last.fm/api/show/radio.getPlaylist>
getPlaylist ∷ Maybe Discovery
            → Maybe RTP
            → Maybe BuyLinks
            → Multiplier
            → Bitrate
            → APIKey
            → SessionKey
            → Secret
            → Lastfm Response

-- | Resolve the name of a resource into a station depending on which resource it is most likely to represent.
--
-- More: <http://www.last.fm/api/show/radio.search>
search ∷ Name → APIKey → Lastfm Response

-- | Tune in to a Last.fm radio station.
--
-- More: <http://www.last.fm/api/show/radio.tune>
tune ∷ Maybe Language → Station → APIKey → SessionKey → Secret → Lastfm Response
