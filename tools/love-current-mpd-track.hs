#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative ((<$>), empty)
import Control.Monad (void)
import Data.Aeson
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Libnotify (oneShot)

import qualified Data.Map as M
import qualified Network.Lastfm.JSON.Track as Track
import qualified Network.Lastfm as LFM
import qualified Network.MPD as MPD

getConfig ∷ IO (LFM.APIKey, LFM.SessionKey, LFM.Secret)
getConfig = do
  hd ← getHomeDirectory
  [apiKey, sessionKey, secret] ← map ((!! 1) . splitOn "=" . filter (not . isSpace)) . lines <$> readFile (hd </> ".lastfm.conf")
  return (LFM.APIKey apiKey, LFM.SessionKey sessionKey, LFM.Secret secret)

getInfo ∷ MPD.Song → Maybe (LFM.Artist, LFM.Track)
getInfo s = (\[x,y] → (LFM.Artist x, LFM.Track y)) <$> mapM getTag [MPD.Artist, MPD.Title]
  where getTag tag = head <$> M.lookup tag (MPD.sgTags s)

getSong ∷ IO (Maybe (LFM.Artist, LFM.Track))
getSong = MPD.withMPD currentSongInfo >>= either (error . show) return
  where currentSongInfo = (getInfo =<<) <$> MPD.currentSong

main ∷ IO ()
main = getConfig >>= \(ak,sk,s) → getSong >>= maybe (return ()) (\(a,t) → correct a t ak >>= love a ak sk s)

newtype Correction = Correction { correction ∷ String }
instance FromJSON Correction where
  parseJSON (Object v) = Correction <$> ((v .: "corrections") >>= (.: "correction") >>= (.: "track") >>= (.: "name"))
  parseJSON _ = empty

correct ∷ LFM.Artist → LFM.Track → LFM.APIKey → IO LFM.Track
correct a t ak = either (const t) (maybe t (LFM.Track . correction) . decode) <$> Track.getCorrection a t ak

love ∷ LFM.Artist → LFM.APIKey → LFM.SessionKey → LFM.Secret → LFM.Track → IO ()
love a@(LFM.Artist artist) ak sk s t@(LFM.Track track) = Track.love a t ak sk s >>=
  void . either
    (\e → oneShot "Lastfm" ("Track HAS NOT loved:\n  " ++ show e) "lastfm" Nothing)
    (\_ → oneShot "Lastfm" ("Track loved:\n  " ++ strip artist ++ " - " ++ strip track ++ ".") "lastfm" Nothing)
  where strip ∷ String → String
        strip (x:xs)
          | x == '&' = "&amp;" ++ strip xs
          | otherwise = x : strip xs
        strip [] = []
