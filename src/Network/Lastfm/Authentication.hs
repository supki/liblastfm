{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Authentication where

import Data.Monoid

import Network.Lastfm.Internal


link ∷ Request a f → String
link q = render . unwrap q $ R
  { host = "http://www.last.fm/api/auth/"
  , _method = mempty
  , _query = mempty
  , parse = undefined
  }
{-# INLINE link #-}
