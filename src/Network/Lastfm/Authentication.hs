{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lastfm authentication procedure helpers
--
-- Basically, lastfm provides 3 ways to authenticate user:
--
--  - web application - <http://www.last.fm/api/webauth>
--
--  - desktop application - <http://www.last.fm/api/desktopauth>
--
--  - modile application - <http://www.last.fm/api/mobileauth>
--
-- Note that you can use any of them in your
-- application despite their names
--
-- How to get session key for yourself for debug with GHCi:
--
-- >>> import Network.Lastfm
-- >>> import Network.Lastfm.Authentication
-- >>> :set -XOverloadedStrings
-- >>> lastfm $ getToken <*> apiKey "__API_KEY__" <* json
-- Just (Object fromList [("token",String "__TOKEN__")])
-- >>> putStrLn . link $ apiKey "__API_KEY__" <* token "__TOKEN__"
-- http://www.last.fm/api/auth/?api_key=__API_KEY__&token=__TOKEN__
-- >>> -- Click that link ^^^
-- >>> lastfm . sign "__SECRET__" $ getSession <*> token "__TOKEN__" <*> apiKey "__API_KEY__"  <* json
-- Just (Object fromList [("session",Object fromList [("name",String "__USER__"),("subscriber",String "0"),("key",String "__SESSION_KEY__")])])
module Network.Lastfm.Authentication
  ( -- * Helpers
    getToken, getSession, getMobileSession
  , link
  ) where

import Control.Applicative ((<*))
import Data.Monoid

import Network.Lastfm.Internal
import Network.Lastfm.Request


-- | Get authorization token
getToken :: Request f (APIKey -> Ready)
getToken = api "auth.getToken"
{-# INLINE getToken #-}


-- | Get session key
getMobileSession :: Request f (Username -> Password -> APIKey -> Sign)
getMobileSession = api "auth.getMobileSession" <* post
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
