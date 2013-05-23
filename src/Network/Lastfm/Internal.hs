{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
-- | liblastfm internals
--
-- You shouldn't need to import this module unless you are doing something interesting.
module Network.Lastfm.Internal
  ( Request(..), Format(..), Ready, Sign
  , R(..), wrap, unwrap, render, coerce
    -- * Lenses
  , host, method, query
  ) where

import Control.Applicative
import Data.Monoid

import           Data.ByteString (ByteString)
import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Serialize (Serialize(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Void (absurd)
import           Network.URI (escapeURIChar, isUnreserved)


-- | Lastfm API request data type
--
-- low-level representation
data R (f :: Format) = R
  { _host   :: {-# UNPACK #-} !Text
  , _method :: {-# UNPACK #-} !ByteString
  , _query  :: !(Map Text Text)
  }

-- | Response format: either JSON or XML
data Format = JSON | XML

-- | Request that is ready to be sent
data Ready

-- | Request that requires signing procedure
data Sign


-- | Lastfm API request data type
--
-- @a@ is authentication state. Might be 'Send' which indicates
-- that you may send this request already or 'Sign', when request signature
-- isn't computed yet
--
-- @f@ is response format. liblastfm currently supports 'JSON' or 'XML'
newtype Request f a = Request { unRequest :: Const (Dual (Endo (R f))) a }

instance Functor (Request f) where
  fmap f (Request x) = Request (fmap f x)
  {-# INLINE fmap #-}

instance Contravariant (Request f) where
  contramap f (Request x) = Request (contramap f x)
  {-# INLINE contramap #-}

instance Applicative (Request f) where
  pure x = Request (pure x)
  Request f <*> Request x = Request (f <*> x)
  {-# INLINE (<*>) #-}

-- | Copypaste from "Control.Lens.Internal.Getter"
coerce :: (Contravariant f, Functor f) => f a -> f b
coerce = fmap absurd . contramap absurd
{-# INLINE coerce #-}


-- | Construct String from request for networking
render :: R f -> String
render R { _host = h, _query = q } =
  T.unpack $ mconcat [h, "?", argie q]
 where
  argie = T.intercalate "&" . M.foldrWithKey (\k v m -> T.concat [escape k, "=", escape v] : m) []

  escape = T.concatMap (T.pack . escapeURIChar isUnreserved)


-- | Wrapping to interesting 'Monoid' ('R' -> 'R') instance
wrap :: (R f -> R f) -> Request f a
wrap = Request . Const . Dual . Endo
{-# INLINE wrap #-}


-- | Unwrapping from interesting 'Monoid' ('R' -> 'R') instance
unwrap :: Request f a -> R f -> R f
unwrap = appEndo . getDual . getConst . unRequest
{-# INLINE unwrap #-}


-- Miscellaneous instances

instance Serialize (R f) where
  put r = do
    put $ T.encodeUtf8 (_host r)
    put $ _method r
    put $ mapmap T.encodeUtf8 T.encodeUtf8 (_query r)
  get = do
    h <- T.decodeUtf8 <$> get
    m <- get
    q <- mapmap T.decodeUtf8 T.decodeUtf8 <$> get
    return R { _host = h, _method = m, _query = q }

mapmap :: (Ord s, Ord t) => (s -> t) -> (a -> b) -> Map s a -> Map t b
mapmap f g = M.mapKeys f . M.map g
{-# INLINE mapmap #-}


-- | 'Request' '_host'
host :: Functor f => (Text -> f Text) -> R h -> f (R h)
host f r@R { _host = h } = (\h' -> r { _host = h' }) <$> f h
{-# INLINE host #-}

-- | 'Request' HTTP '_method'
method :: Functor f => (ByteString -> f ByteString) -> R h -> f (R h)
method f r@R { _method = m } = (\m' -> r { _method = m' }) <$> f m
{-# INLINE method #-}

-- | 'Request' '_query' string
query :: Functor f => (Map Text Text -> f (Map Text Text)) -> R h -> f (R h)
query f r@R { _query = q } = (\q' -> r { _query = q' }) <$> f q
{-# INLINE query #-}
