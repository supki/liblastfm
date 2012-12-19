{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Lastfm authentication procedure helpers
--
-- Basically, lastfm provides 3 ways to authenticate user:
--
--  - web application - <http://www.lastfm.ru/api/webauth>
--
--  - desktop application - <http://www.lastfm.ru/api/desktopauth>
--
--  - modile application - <http://www.lastfm.ru/api/mobileauth>
--
-- Note that you can use any of them in your
-- application despite their names
module Network.Lastfm.Authentication
  ( -- * Helpers
    getToken, getSession, getMobileSession
  , link
  ) where

import Data.Monoid

import Network.Lastfm.Internal
import Network.Lastfm.Request


-- | Get authorization token
getToken ∷ Request f Send (APIKey → Ready)
getToken = api "auth.getToken"
{-# INLINE getToken #-}


-- | Get session key
getMobileSession ∷ Request f Sign (Username → Password → APIKey → Ready)
getMobileSession = api "auth.getMobileSession"
{-# INLINE getMobileSession #-}


-- | Get session key
getSession ∷ Request f Sign (Token → APIKey → Ready)
getSession = api "auth.getSession"
{-# INLINE getSession #-}


-- | Construct link user should follow to approve application
link ∷ Request f a t → String
link q = render . unwrap q $ R
  { host = "http://www.last.fm/api/auth/"
  , method = mempty
  , query = mempty
  , parse = undefined
  }
{-# INLINE link #-}
