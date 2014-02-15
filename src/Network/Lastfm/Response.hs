{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | Request sending and Response parsing
module Network.Lastfm.Response
  ( -- * Request signature
    -- $sign
    Secret(..), sign
    -- * Get 'Response'
  , Response, Supported, Format(..), lastfm, lastfm_
    -- ** Errors
  , LastfmError(..)
  , _LastfmBadResponse
  , _LastfmEncodedError
  , _LastfmStatusCodeError
  , _LastfmHttpError
    -- ** Internal
  , lastfm', finalize
#ifdef TEST
  , parse
  , md5
#endif
  ) where

import           Control.Applicative
import           Control.Exception (SomeException(..), Exception(..), try, throw)
import           Crypto.Classes (hash')
import           Data.Aeson ((.:), Value(..), decode)
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import           Data.Digest.Pure.MD5 (MD5Digest)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Proxy (Proxy(..))
import           Data.Profunctor (Choice, dimap, right')
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Data.Typeable (Typeable, cast)
import qualified Network.HTTP.Conduit as N
import qualified Network.HTTP.Types as N
import           Text.XML (Document, parseLBS, def)
import           Text.XML.Cursor

import           Network.Lastfm.Internal


-- $sign
--
-- Signature is required for every
-- authenticated API request. Basically,
-- every such request appends the md5 footprint
-- of its arguments to the query as
-- described at <http://www.last.fm/api/authspec#8>


-- | 'Supported' provides parsing for a chosen 'Format'
--
-- 'JSON' is parsed to aeson's 'Value', 'XML' is to lazy 'ByteString'
-- (in other words, parsing XML is left to the user)
class Supported (f :: Format) where
  type Response f
  parse :: proxy f -> Lazy.ByteString -> Int -> Response f
  errorResponse :: proxy f -> Response f -> Maybe LastfmError
  base :: R f

instance Supported JSON where
  type Response JSON = Value

  parse p body code = case decode body of
    Just v
      | Just e <- errorResponse p v -> throw e
      | code /= 200                -> throw (LastfmStatusCodeError code body)
      | otherwise                  -> v
    Nothing -> throw (LastfmBadResponse body)

  errorResponse _ = parseMaybe $ \(Object o) -> do
    code <- o .: "error"
    msg  <- o .: "message"
    return (LastfmEncodedError code msg)

  base = R
    { _host   = "https://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query  = M.fromList [("format", "json")]
    }

instance Supported XML where
  type Response XML = Document

  parse p body code = case parseLBS def body of
    Right v
      | Just e <- errorResponse p v -> throw e
      |  code /= 200               -> throw (LastfmStatusCodeError code body)
      | otherwise                  -> v
    Left _ -> throw (LastfmBadResponse body)

  errorResponse _ doc = case fromDocument doc of
    cur
      | [mcode]         <- cur $| element "lfm" >=> child >=> element "error" >=> attribute "code"
      , Right (code, _) <- T.decimal mcode
      , [msg]           <- cur $| element "lfm" >=> child >=> element "error" >=> child >=> content
      -> Just (LastfmEncodedError code (T.strip msg))
      |
      otherwise -> Nothing

  base = R
    { _host   = "https://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query  = mempty
    }

-- | Different ways last.fm response can be unusable
data LastfmError =
    -- | last.fm thinks it responded with something legible, but it really isn't
    LastfmBadResponse Lazy.ByteString
    -- | last.fm error code and message string
  | LastfmEncodedError Int Text
    -- | last.fm returns non-200 status code
  | LastfmStatusCodeError Int Lazy.ByteString
    -- | http-conduit exception, wrapped
  | LastfmHttpError N.HttpException
    deriving (Show, Typeable)

-- | Admittedly, this isn't the best 'Eq' instance ever
-- but not having 'Eq' 'C.HttpException' does not leave much a choice
instance Eq LastfmError where
  LastfmBadResponse bs      == LastfmBadResponse bs'      = bs == bs'
  LastfmEncodedError e s    == LastfmEncodedError e' t    = e == e' && s == t
  LastfmStatusCodeError e s == LastfmStatusCodeError e' t = e == e' && s == t
  LastfmHttpError _         == LastfmHttpError _          = True
  _                         == _                          = False

instance Exception LastfmError where
  fromException e@(SomeException se)
    | Just e' <- fromException e = Just (LastfmHttpError e')
    | otherwise                 = cast se

class AsLastfmError t where
  _LastfmError :: (Choice p, Applicative m) => p LastfmError (m LastfmError) -> p t (m t)

instance AsLastfmError LastfmError where
  _LastfmError = id
  {-# INLINE _LastfmError #-}

instance AsLastfmError SomeException where
  _LastfmError = dimap (\e -> maybe (Left e) Right (fromException e)) (either pure (fmap toException)) . right'
  {-# INLINE _LastfmError #-}

-- | This is a @ Prism' 'LastfmError' 'Lazy.ByteString' @ in disguise
_LastfmBadResponse
  :: (Choice p, Applicative m, AsLastfmError e)
  => p Lazy.ByteString (m Lazy.ByteString) -> p e (m e)
_LastfmBadResponse = _LastfmError . dimap go (either pure (fmap LastfmBadResponse)) . right' where
  go (LastfmBadResponse bs) = Right bs
  go x                      = Left x
  {-# INLINE go #-}
{-# INLINE _LastfmBadResponse #-}

-- | This is a @ Prism' 'LastfmError' ('Int', 'String') @ in disguise
_LastfmEncodedError
  :: (Choice p, Applicative m, AsLastfmError e)
  => p (Int, Text) (m (Int, Text)) -> p e (m e)
_LastfmEncodedError = _LastfmError . dimap go (either pure (fmap (uncurry LastfmEncodedError))) . right' where
  go (LastfmEncodedError n v) = Right (n, v)
  go x                        = Left x
  {-# INLINE go #-}
{-# INLINE _LastfmEncodedError #-}

-- | This is a @ Prism' 'LastfmError' ('Int', 'Lazy.ByteString') @ in disguise
_LastfmStatusCodeError
  :: (Choice p, Applicative m, AsLastfmError e)
  => p (Int, Lazy.ByteString) (m (Int, Lazy.ByteString)) -> p e (m e)
_LastfmStatusCodeError = _LastfmError . dimap go (either pure (fmap (uncurry LastfmStatusCodeError))) . right' where
  go (LastfmStatusCodeError e s) = Right (e, s)
  go x                           = Left x
  {-# INLINE go #-}
{-# INLINE _LastfmStatusCodeError #-}

-- | This is a @ Prism' 'LastfmError' 'C.HttpException' @ in disguise
_LastfmHttpError
  :: (Choice p, Applicative m, AsLastfmError e)
  => p N.HttpException (m N.HttpException) -> p e (m e)
_LastfmHttpError = _LastfmError . dimap go (either pure (fmap LastfmHttpError)) . right' where
  go (LastfmHttpError e) = Right e
  go x                   = Left x
  {-# INLINE go #-}
{-# INLINE _LastfmHttpError #-}


-- | Application secret
newtype Secret = Secret Text deriving (Show, Eq, Typeable)

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
  signer = md5 . M.foldrWithKey (\k v xs -> k <> v <> xs) s

-- | Get supplied string md5 hash hex representation
md5 :: Text -> Text
md5 = T.pack . show . (hash' :: Strict.ByteString -> MD5Digest) . T.encodeUtf8


-- | Send 'Request' and parse the 'Response'
--
-- Also catches 'C.HttpException's thrown by http-conduit
lastfm :: Supported f => Request f Ready -> IO (Either LastfmError (Response f))
lastfm = try . lastfm' parse (\_ _ _ -> Nothing) . finalize

-- | Send 'Request' without parsing the 'Response'
--
-- Does not interfere with exceptions in any way
lastfm_ :: Supported f => Request f Ready -> IO ()
lastfm_ = lastfm' (\_ _ _ -> ()) (N.checkStatus def) . finalize

-- | Send 'R' and parse 'Response' with the supplied function
lastfm'
  :: Supported f
  => (Proxy f -> Lazy.ByteString -> Int -> a)
  -> (N.Status -> [N.Header] -> N.CookieJar -> Maybe SomeException)
  -> R f
  -> IO a
lastfm' parser handler request = N.withManager $ \manager -> do
  req <- N.parseUrl (render request)
  let req' = req
       { N.method          = _method request
       , N.responseTimeout = Just 10000000
       , N.checkStatus     = handler
       }
  res <- N.httpLbs req' manager
  return $ parser Proxy (N.responseBody res) (N.statusCode (N.responseStatus res))

-- | Get 'R' from 'Request'
finalize :: Supported f => Request f Ready -> R f
finalize = ($ base) . unwrap
