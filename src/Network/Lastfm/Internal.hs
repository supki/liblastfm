{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
-- | liblastfm internals
--
-- You shouldn't need to import this module unless you are doing something interesting.
module Network.Lastfm.Internal
  ( Request(..)
  , Format(..)
  , Ready
  , Sign
  , R(..)
  , wrap
  , unwrap
  , render
  , coerce
  , absorbQuery
  , indexedWith
    -- * Lenses
  , host
  , method
  , query
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Foldable (Foldable(..))
import           Data.Functor.Contravariant (Contravariant(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Serialize (Serialize(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable (Traversable(..))
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
-- @a@ is the authentication state. Can be 'Ready', which means this 'Request' is
-- ready to be sent, or 'Sign', if the request signature hasn't been computed yet
--
-- @f@ is the response format (liblastfm supports both 'JSON' and 'XML')
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

instance Foldable (Request f) where
  foldMap _ (Request _) = mempty -- not sure why this instance isn't in base
  {-# INLINE foldMap #-}

instance Traversable (Request f) where
  traverse _ (Request (Const x)) = pure (Request (Const x)) -- and that
  {-# INLINE traverse #-}


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


-- | Absorbing a bunch of queries, useful in batch operations
absorbQuery :: Foldable t => t (Request f b) -> Request f a
absorbQuery rs = wrap $ \r ->
  r { _query = _query r <> foldMap (_query . ($ rempty) . unwrap) rs }
{-# INLINE absorbQuery #-}

-- | Transforming Request to the "array notation"
indexedWith :: Int -> Request f a -> Request f a
indexedWith n r = r <* wrap (\s ->
  s { _query = M.mapKeys (\k -> k <> "[" <> T.pack (show n) <> "]") (_query s) })
{-# INLINE indexedWith #-}

-- | Empty request
rempty :: R f
rempty = R mempty mempty mempty
{-# INLINE rempty #-}


-- Miscellaneous instances

instance Serialize (R f) where
  put r = do
    put $ T.encodeUtf8 (_host r)
    put $ _method r
    put $ bimap T.encodeUtf8 T.encodeUtf8 (_query r)
  get = do
    h <- T.decodeUtf8 <$> get
    m <- get
    q <- bimap T.decodeUtf8 T.decodeUtf8 <$> get
    return R { _host = h, _method = m, _query = q }

bimap :: (Ord s, Ord t) => (s -> t) -> (a -> b) -> Map s a -> Map t b
bimap f g = M.mapKeys f . M.map g
{-# INLINE bimap #-}


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
