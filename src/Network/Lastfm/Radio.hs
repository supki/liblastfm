{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lastfm radio API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Radio as Radio
-- @
module Network.Lastfm.Radio
  ( getPlaylist, search, tune
  ) where

import Control.Applicative

import Network.Lastfm.Request


-- | Fetch new radio content periodically in an XSPF format.
--
-- Optional: 'discovery', 'rtp', 'buyLinks'
--
-- <http://www.last.fm/api/show/radio.getPlaylist>
getPlaylist :: Request f (Multiplier -> Bitrate -> APIKey -> SessionKey -> Sign)
getPlaylist = api "radio.getPlaylist"
{-# INLINE getPlaylist #-}


-- | Resolve the name of a resource into a station depending on which resource
-- it is most likely to represent.
--
-- <http://www.last.fm/api/show/radio.search>
search :: Request f (Name -> APIKey -> Ready)
search = api "radio.search"
{-# INLINE search #-}


-- | Tune in to a Last.fm radio station.
--
-- Optional: 'language'
--
-- <http://www.last.fm/api/show/radio.tune>
tune :: Request f (Station -> APIKey -> SessionKey -> Sign)
tune = api "radio.tune" <* post
{-# INLINE tune #-}
