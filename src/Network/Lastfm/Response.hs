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
  , LastfmError(..), _LastfmBadResponse, _LastfmEncodedError, _LastfmHttpException
    -- ** Internal
  , lastfm', finalize
#ifdef TEST
  , parse
  , md5
  , wrapHttpException
#endif
  ) where

import           Control.Applicative
import           Control.Exception (handle)
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
import qualified Network.HTTP.Conduit as C
import           Text.XML (Document, parseLBS, def)

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
  parse :: proxy f -> Lazy.ByteString -> Either LastfmError (Response f)
  base :: R f

instance Supported JSON where
  type Response JSON = Value
  parse _ b = case decode b of
    Just v -> case parseMaybe errorParser v of
      Just e  -> Left e
      Nothing -> Right v
    Nothing -> Left (LastfmBadResponse b)
   where
    errorParser (Object o) = do
      code <- o .: "error"
      msg  <- o .: "message"
      return (LastfmEncodedError code msg)
    errorParser _ = empty
  base = R
    { _host   = "https://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query  = M.fromList [("format", "json")]
    }
  {-# INLINE base #-}

instance Supported XML where
  type Response XML = Document
  parse _ b = case parseLBS def b of
    Left _ ->
      Left (LastfmBadResponse b)
    Right v ->
      Right v
  {-# INLINE parse #-}
  base = R
    { _host   = "https://ws.audioscrobbler.com/2.0/"
    , _method = "GET"
    , _query  = mempty
    }
  {-# INLINE base #-}

-- | Different ways last.fm response can be unusable
data LastfmError =
    -- | last.fm thinks it responded with something legible, but it really isn't
    LastfmBadResponse Lazy.ByteString
    -- | last.fm error code and message string
  | LastfmEncodedError Int String
    -- | http-conduit exception, wrapped
  | LastfmHttpException C.HttpException
    deriving (Show)

-- | Admittedly, this isn't the best 'Eq' instance ever
-- but not having 'Eq' 'C.HttpException' does not leave much a choice
instance Eq LastfmError where
  LastfmBadResponse bs   == LastfmBadResponse bs'   = bs == bs'
  LastfmEncodedError e s == LastfmEncodedError e' t = e == e' && s == t
  LastfmHttpException _  == LastfmHttpException _   = True
  _                      == _                       = False

-- | This is a @ Prism' 'LastfmError' 'Lazy.ByteString' @ in disguise
_LastfmBadResponse
  :: (Choice p, Applicative m)
  => p Lazy.ByteString (m Lazy.ByteString) -> p LastfmError (m LastfmError)
_LastfmBadResponse = dimap go (either pure (fmap LastfmBadResponse)) . right'
 where
  go (LastfmBadResponse bs) = Right bs
  go x                      = Left x
  {-# INLINE go #-}
{-# INLINE _LastfmBadResponse #-}

-- | This is a @ Prism' 'LastfmError' 'Int' @ in disguise
_LastfmEncodedError
  :: (Choice p, Applicative m)
  => p (Int, String) (m (Int, String)) -> p LastfmError (m LastfmError)
_LastfmEncodedError = dimap go (either pure (fmap (uncurry LastfmEncodedError))) . right'
 where
  go (LastfmEncodedError n v) = Right (n, v)
  go x                        = Left x
  {-# INLINE go #-}
{-# INLINE _LastfmEncodedError #-}

-- | This is a @ Prism' 'LastfmError' 'C.HttpException' @ in disguise
_LastfmHttpException
  :: (Choice p, Applicative m)
  => p C.HttpException (m C.HttpException) -> p LastfmError (m LastfmError)
_LastfmHttpException = dimap go (either pure (fmap LastfmHttpException)) . right'
 where
  go (LastfmHttpException e) = Right e
  go x                       = Left x
  {-# INLINE go #-}
{-# INLINE _LastfmHttpException #-}


-- | Application secret
newtype Secret = Secret Text deriving (Show, Eq)

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
lastfm = wrapHttpException . lastfm' parse . finalize

-- | Send 'Request' without parsing the 'Response'
lastfm_ :: Supported f => Request f Ready -> IO ()
lastfm_ = lastfm' (\_ _ -> ()) . finalize

-- | Handle http-conduit's 'C.HttpException' by wrapping it
wrapHttpException :: IO (Either LastfmError a) -> IO (Either LastfmError a)
wrapHttpException = handle (return . Left . LastfmHttpException)

-- | Send 'R' and parse 'Response' with the supplied function
lastfm' :: Supported f => (Proxy f -> Lazy.ByteString -> a) -> R f -> IO a
lastfm' f request = C.withManager $ \manager -> do
  req <- C.parseUrl (render request)
  let req' = req
       { C.method          = _method request
       , C.responseTimeout = Just 10000000
       }
  res <- C.httpLbs req' manager
  return $ f Proxy (C.responseBody res)

-- | Get 'R' from 'Request'
finalize :: Supported f => Request f Ready -> R f
finalize = ($ base) . unwrap
