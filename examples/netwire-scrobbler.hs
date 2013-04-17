{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (catch)
import Control.Monad (void)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Prelude hiding ((.), (**), id)

import           Control.Monad.Trans (MonadIO, liftIO)
import           Control.Wire
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Time (formatTime, getCurrentTime)
import           Network.Lastfm hiding (Track, tags)
import qualified Network.Lastfm.Track as T
import qualified Network.MPD as Y
import           System.Locale (defaultTimeLocale)


ak :: Request f a APIKey
ak = apiKey "__YOUR_API_KEY__"

sk :: Request f Sign SessionKey
sk = sessionKey "__YOUR_SESSION_KEY__"

secret :: Secret
secret = "__YOUR_SECRET__"


-- | Player state change
type Change = Maybe Track

-- | Track information
data Track = Track
  { _timestamp :: Int64  -- ^ playing start timestamp
  , _artist    :: Text   -- ^ artist
  , _title     :: Text   -- ^ title
  , _album     :: Text   -- ^ album title (optional)
  , _length    :: Int64  -- ^ duration
  } deriving (Show, Read, Eq, Ord)

-- | Scrobble state
data Scrobble
  = Started Track -- ^ Candidate for scrobbling record
  | Idle          -- ^ Doing nothing currently
    deriving (Show, Read, Eq, Ord)

-- | Player state
data Player
  = Playing Y.Song -- ^ Candidate for scrobbling record
  | Stopped        -- ^ Being stopped
  | Paused         -- ^ Being paused
    deriving (Show, Eq)


-- | Scrobbler errors
data Error
  = NoChange
  | NoScrobble
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- | Application loop
main :: IO ()
main =
  void (Y.withMPD (loop' (scrobble . time ** announce . change . time) clockSession))
 `catch`
  \(_ :: SomeException) -> main
 where
  loop' :: Wire Error Y.MPD () Track -> Session Y.MPD -> Y.MPD ()
  loop' w' session' = do
    (mx, w, session) <- stepSession w' session' ()
    case mx of
      Right x -> go x
      _       -> return Nothing
    loop' w session

  go Track { _artist = ar, _title = t, _album = al } = do
    ts <- read . formatTime defaultTimeLocale "%s" <$> io getCurrentTime
    io . lastfm . sign secret $
      T.scrobble <*> artist ar <*> track t <*> timestamp ts <* album al <*> ak <*> sk <* json


-- | Look for player state changes over time
change :: Wire Error Y.MPD Time Change
change = mkStateM Stopped $ \_dt (t, s) ->
  Y.stState `fmap` Y.status >>= \s' -> case (s, s') of
    (Paused,    Y.Stopped) -> return (Right Nothing, Stopped)
    (Playing _, Y.Stopped) -> return (Right Nothing, Stopped)
    (Stopped,   Y.Paused)  -> return (Right Nothing, Paused)
    (Playing _, Y.Paused)  -> return (Right Nothing, Paused)
    (Playing song, Y.Playing) -> do
      Just song' <- Y.currentSong
      if song /= song' then do
        return (Right (Just (fetch t song')), Playing song')
      else return (Left NoChange, s)
    (_, Y.Playing) -> do
      Just song <- Y.currentSong
      return (Right (Just (fetch t song)), Playing song)
    _ -> return (Left NoChange, s)
 where
  fetch t song = let tags = Y.sgTags song in Track
    { _timestamp = round t
    , _artist    = fromMaybe "" $ Y.toText . head <$> M.lookup Y.Artist tags
    , _title     = fromMaybe "" $ Y.toText . head <$> M.lookup Y.Title tags
    , _album     = fromMaybe "" $ Y.toText . head <$> M.lookup Y.Album tags
    , _length    = fromIntegral $ Y.sgLength song
    }


-- | Announce player state change to the whole world
announce :: MonadIO m => Wire e m Change Change
announce = mkFixM $ \_dt ch -> case ch of
  Nothing -> return (Right ch)
  Just tr -> go tr >> return (Right ch)
 where
  go Track { _artist = ar, _title = t, _album = al, _length = l } = io . lastfm . sign secret $
    T.updateNowPlaying <*> artist ar <*> track t <* album al <* duration l <*> ak <*> sk <* json


-- | Since player state has changed, probably scrobble is needed
scrobble :: Wire Error Y.MPD (Time, Change) Track
scrobble = mkState Idle $ \_dt ((t, tr), s) ->
  let s' = maybe Idle Started tr in  case s of
    Idle        -> (Left NoScrobble, s')
    Started tr'
      | (round t - _timestamp tr') * 2 > _length tr' -> (Right tr', s')
      | otherwise                                  -> (Left NoScrobble, s')


io :: MonadIO m => IO a -> m a
io = liftIO


infixr 9 **
(**) :: Applicative m => m a -> m b -> m (a, b)
(**) = liftA2 (,)
