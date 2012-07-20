{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Playlist API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Playlist
  ( addTrack, create
  ) where

#include "playlist.docs"

import Network.Lastfm.Internal
import Network.Lastfm.JSON (jsonWrapper)
import qualified Network.Lastfm.API.Playlist as API

$(jsonWrapper ["addTrack", "create"])

__addTrack__
addTrack ∷ Playlist → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

__create__
create ∷ Maybe Title → Maybe Description → APIKey → SessionKey → Secret → Lastfm Response
