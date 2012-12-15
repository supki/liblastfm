{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Tasteometer where

import Data.Monoid ((<>))

import Network.Lastfm.Request


compare ∷ User → User → Request f Ready t
compare u1 u2 = api "tasteometer.compare" <> type' 1 "user" <> value 1 u1 <> type' 2 "user" <> value 2 u2
