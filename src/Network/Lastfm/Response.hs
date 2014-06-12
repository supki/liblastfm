{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Request sending and Response parsing
module Network.Lastfm.Response
  ( -- * Compute request signature
    -- $sign
    Secret(..)
  , sign
    -- * Get response
  , Supported
  , Format(..)
  , lastfm
  , lastfm_
    -- ** Errors
  , LastfmError(..)
  , _LastfmBadResponse
  , _LastfmEncodedError
  , _LastfmHttpError
    -- ** Internal
  , lastfmWith
  , finalize
#ifdef TEST
  , parse
  , md5
#endif
  ) where

import           Control.Applicative
import           Control.Exception (SomeException(..), Exception(..), catch)
import           Crypto.Classes (hash')
import           Data.Aeson ((.:), Value(..), decode)
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import           Data.Digest.Pure.MD5 (MD5Digest)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Profunctor (Choice, dimap, right')
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Data.Typeable (Typeable, cast)
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N
import           Text.XML (Document, parseLBS, def)
import           Text.XML.Cursor

import           Network.Lastfm.Internal


-- $sign
--
-- The signature is required for every
-- authenticated API request. Basically,
-- every such request appends the md5 footprint
-- of its arguments to the query as
-- described at <http://www.last.fm/api/authspec#8>


-- | 'Supported' provides parsing for the chosen 'Format'
--
-- 'JSON' is parsed to 'Value' type from aeson, while 'XML'
-- is parsed to 'Document' from xml-conduit
class Supported f r | f -> r, r -> f where
  prepareRequest :: R f -> R f
  parseResponseBody :: Lazy.ByteString -> Maybe r
  parseResponseEncodedError :: r -> Maybe LastfmError

instance Supported JSON Value where
  prepareRequest r = r { _query = M.singleton "format" "json" `M.union` _query r }
  parseResponseBody = decode
  parseResponseEncodedError = parseMaybe $ \(Object o) -> do
    code <- o .: "error"
    msg  <- o .: "message"
    return (LastfmEncodedError code msg)

instance Supported XML Document where
  prepareRequest = id
  parseResponseBody = either (const Nothing) Just . parseLBS def
  parseResponseEncodedError doc = case fromDocument doc of
    cur
      | [mcode]         <- cur $| element "lfm" >=> child >=> element "error" >=> attribute "code"
      , Right (code, _) <- T.decimal mcode
      , [msg]           <- cur $| element "lfm" >=> child >=> element "error" >=> child >=> content
      -> Just (LastfmEncodedError code (T.strip msg))
      |
      otherwise -> Nothing

parse :: Supported f r => Lazy.ByteString -> Either LastfmError r
parse body = case parseResponseBody body of
  Just v
    | Just e <- parseResponseEncodedError v -> Left e
    | otherwise -> Right v
  Nothing -> Left (LastfmBadResponse body)

base :: R f
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
    -- | wrapped http-conduit exception
  | LastfmHttpError N.HttpException
    deriving (Show, Typeable)

-- | Admittedly, this isn't the best 'Eq' instance ever
-- but not having 'Eq' 'C.HttpException' does not leave much a choice
instance Eq LastfmError where
  LastfmBadResponse bs      == LastfmBadResponse bs'      = bs == bs'
  LastfmEncodedError e s    == LastfmEncodedError e' t    = e == e' && s == t
  LastfmHttpError _         == LastfmHttpError _          = True
  _                         == _                          = False

instance Exception LastfmError where
  fromException e@(SomeException se)
    | Just e' <- fromException e = Just (LastfmHttpError e')
    | otherwise                  = cast se

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

-- | Sign the 'Request' with the 'Secret' so it's ready to be sent
sign :: Secret -> Request f Sign -> Request f Ready
sign s = coerce . (<* signature)
 where
  signature = wrap $
    \r@R { _query = q } -> r { _query = apiSig s . authToken $ q }

authToken :: Map Text Text -> Map Text Text
authToken q = maybe q (M.delete "password") $ do
  password <- M.lookup "password" q
  username <- M.lookup "username" q
  return (M.insert "authToken" (md5 (username <> md5 password)) q)

apiSig :: Secret -> Map Text Text -> Map Text Text
apiSig (Secret s) q = M.insert "api_sig" (signer (foldr M.delete q ["format", "callback"])) q
 where
  signer = md5 . M.foldrWithKey (\k v xs -> k <> v <> xs) s

-- | Get supplied string md5 hash hex representation
md5 :: Text -> Text
md5 = T.pack . show . (hash' :: Strict.ByteString -> MD5Digest) . T.encodeUtf8


-- | Send the 'Request' and parse the 'Response'
lastfm :: Supported f r => Request f Ready -> IO (Either LastfmError r)
lastfm = lastfmWith parse . finalize

-- | Send the 'Request' without parsing the 'Response'
lastfm_ :: Supported f r => Request f Ready -> IO (Either LastfmError ())
lastfm_ = lastfmWith (\_ -> Right ()) . finalize

-- | Send the 'R' and parse the 'Response' with the supplied parser
lastfmWith
  :: Supported f r
  => (Lazy.ByteString -> Either LastfmError a)
  -> R f
  -> IO (Either LastfmError a)
lastfmWith p r = N.withManager N.tlsManagerSettings $ \manager -> do
  req <- N.parseUrl (render r)
  let req' = req
       { N.method          = _method r
       , N.responseTimeout = Just 10000000
       }
  p . N.responseBody <$> N.httpLbs req' manager
 `catch`
  (return . Left)

-- | Get the 'R' from the 'Request'
finalize :: Supported f r => Request f Ready -> R f
finalize x = (prepareRequest . unwrap x) base
