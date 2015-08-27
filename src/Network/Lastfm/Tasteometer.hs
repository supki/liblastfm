{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lastfm tasteometer API
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified Network.Lastfm.Tasteometer as Tasteometer
-- @
module Network.Lastfm.Tasteometer where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Network.Lastfm.Request


-- | Get a Tasteometer score from two inputs, along with a list of shared artists.
-- If the input is a user some additional information is returned.
--
-- Optional: 'limit'
--
-- <http://www.last.fm/api/show/tasteometer.compare>
compare :: (Targeted u, Targeted v) => Request f u -> Request f v -> Request f (APIKey -> Ready)
compare u v = api "tasteometer.compare" <* comparison 1 u <* comparison 2 v
