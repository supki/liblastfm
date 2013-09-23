{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | Request sending and Response parsing
module Network.Lastfm.Response
  ( -- * Sign Request
    -- $sign
    Secret(..), sign
    -- * Get Response
  , Response, lastfm, lastfm', finalize
  ) where

import Control.Applicative
import Control.Exception (throw)
import Control.Monad
import Data.Monoid
import Data.String (IsString(..))

import           Crypto.Classes (hash')
import           Data.Aeson ((.:), Value, decode, parseJSON)
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import           Data.Digest.Pure.MD5 (MD5Digest)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C

import Network.Lastfm.Internal


-- $sign
--
-- Signing is important part of every
-- authentication requiring API request.
-- Basically, every such request is appended
-- with md5 footprint of its arguments as
-- described at <http://www.last.fm/api/authspec#8>


class Supported (f :: Format) where
  type Response f
  parse :: R f -> Lazy.ByteString -> C.ResponseHeaders -> Response f
  base :: R f


instance Supported JSON where
  type Response JSON = Maybe Value
  parse _ b hs = do
    v <- decode b
    case parseMaybe ((.: "error") <=< parseJSON) v :: Maybe Int of
      Just _ ->
        throw (C.StatusCodeException C.status400 (("Response", Strict.concat $ Lazy.toChunks b) : hs) (C.createCookieJar []))
      _ -> return v
  base = R
    { _host = "https://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query = M.fromList [("format", "json")]
    }
  {-# INLINE base #-}

instance Supported XML where
  type Response XML = Lazy.ByteString
  parse _ b _ = b
  {-# INLINE parse #-}
  base = R
    { _host = "https://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query = mempty
    }
  {-# INLINE base #-}


-- | Application secret
newtype Secret = Secret Text deriving (Show)

instance IsString Secret where
  fromString = Secret . fromString

-- | Sign 'Request' with 'Secret'
sign :: Secret -> Request f Sign -> Request f Ready
sign s = coerce . (<* signature)
 where
  signature = wrap $
    \r@R { _query = q } -> r { _query = api_sig s . authToken $ q }

authToken :: Map Text Text -> Map Text Text
authToken q = maybe q (M.delete "password") $ do
  password <- M.lookup "password" q
  username <- M.lookup "username" q
  return (M.insert "authToken" (md5 (username <> (md5 password))) q)

api_sig :: Secret -> Map Text Text -> Map Text Text
api_sig (Secret s) q = M.insert "api_sig" (signer (foldr M.delete q ["format", "callback"])) q
 where
  signer = md5 . M.foldrWithKey(\k v xs -> k <> v <> xs) s

md5 :: Text -> Text
md5 = T.pack . show . (hash' :: Strict.ByteString -> MD5Digest) . T.encodeUtf8


-- | Send Request and parse Response
lastfm :: Supported f => Request f Ready -> IO (Response f)
lastfm = lastfm' . finalize


-- | Get R from Request
--
-- That's rarely needed unless you want low-level requests manipulation
finalize :: Supported f => Request f Ready -> R f
finalize = ($ base) . unwrap


-- | Send R and parse Response
--
-- That's rarely needed unless you want low-level requests manipulation
lastfm' :: Supported f => R f -> IO (Response f)
lastfm' request = C.withManager $ \manager -> do
  req <- C.parseUrl (render request)
  let req' = req
       { C.method          = _method request
       , C.responseTimeout = Just 10000000
       }
  res <- C.httpLbs req' manager
  return $ parse request (C.responseBody res) (C.responseHeaders res)
