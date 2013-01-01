{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Tasteometer where

import Control.Applicative

import Network.Lastfm.Request


-- | Get a Tasteometer score from two inputs, along with a list of shared artists.
-- If the input is a user some additional information is returned.
--
-- Optional: 'limit'
--
-- <http://www.lastfm.ru/api/show/tasteometer.compare>
compare ∷ (Targeted u, Targeted v) ⇒ Request f Send u → Request f Send v → Request f Send (APIKey → Ready)
compare u v = api "tasteometer.compare" <* comparison 1 u <* comparison 2 v
{-# INLINE compare #-}
