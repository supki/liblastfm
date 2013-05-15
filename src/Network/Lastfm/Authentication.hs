{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
getToken :: Request f (APIKey -> Ready)
getToken = api "auth.getToken"
{-# INLINE getToken #-}


-- | Get session key
getMobileSession :: Request f (Username -> Password -> APIKey -> Sign)
getMobileSession = api "auth.getMobileSession"
{-# INLINE getMobileSession #-}


-- | Get session key
getSession :: Request f (Token -> APIKey -> Sign)
getSession = api "auth.getSession"
{-# INLINE getSession #-}


-- | Construct link user should follow to approve application
link :: Request f a -> String
link q = render . unwrap q $ R
  { _host = "http://www.last.fm/api/auth/"
  , _method = mempty
  , _query = mempty
  }
{-# INLINE link #-}
