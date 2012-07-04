{-# LANGUAGE TemplateHaskell #-}
-- | Event API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Event
  ( attend, getAttendees, getInfo, getShouts, share, shout
  ) where

#include "event.docs"

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Event as API

$(json ["attend", "getAttendees", "getInfo", "getShouts", "share", "shout"])

__attend__
attend ∷ Event → Status → APIKey → SessionKey → Secret → Lastfm Response

__getAttendees__
getAttendees ∷ Event → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getInfo__
getInfo ∷ Event → APIKey → Lastfm Response

__getShouts__
getShouts ∷ Event → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__share__
share ∷ Event → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response

__shout__
shout ∷ Event → Message → APIKey → SessionKey → Secret → Lastfm Response
