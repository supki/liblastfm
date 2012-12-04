{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.User
  ( getFriends
  ) where

import Data.Monoid ((<>))

import Network.Lastfm.Request


-- | Get a list of the user's friends on Last.fm
--
-- <http://www.last.fm/api/show/user.getFriends>
getFriends ∷ User → Request Ready f
getFriends u = api "user.getFriends" <> user u
