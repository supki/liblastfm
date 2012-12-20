{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Tasteometer where

import Control.Applicative

import Network.Lastfm.Request


compare ∷ Request f Send (Value' → Value' → APIKey → Ready)
compare = api "tasteometer.compare" <* type' 1 "user" <* type' 2 "user"
