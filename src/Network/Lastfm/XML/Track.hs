{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Track API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.XML.Track
  ( addTags, ban, getBuyLinks, getCorrection, getFingerprintMetadata
  , getInfo, getShouts, getSimilar, getTags, getTopFans, getTopTags
  , love, removeTag, scrobble, search, share, unban, unlove, updateNowPlaying
  ) where

#include "track.docs"

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Track as API

$(xml ["addTags", "ban", "getBuyLinks", "getCorrection", "getFingerprintMetadata", "getInfo", "getShouts", "getSimilar", "getTags", "getTopFans", "getTopTags", "love", "removeTag", "scrobble", "search", "share", "unban", "unlove", "updateNowPlaying"])

__addTags__
addTags ∷ Artist → Track → [Tag] → APIKey → SessionKey → Secret → Lastfm Response

__ban__
ban ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

__getBuyLinks__
getBuyLinks ∷ Either (Artist, Track) Mbid → Maybe Autocorrect → Country → APIKey → Lastfm Response

__getCorrection__
getCorrection ∷ Artist → Track → APIKey → Lastfm Response

__getFingerprintMetadata__
getFingerprintMetadata ∷ Fingerprint → APIKey → Lastfm Response

__getInfo__
getInfo ∷ Either (Artist, Track) Mbid → Maybe Autocorrect → Maybe Username → APIKey → Lastfm Response

__getShouts__
getShouts ∷ Either (Artist, Track) Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getSimilar__
getSimilar ∷ Either (Artist, Track) Mbid → Maybe Autocorrect → Maybe Limit → APIKey → Lastfm Response

__getTags__
getTags ∷ Either (Artist, Track) Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response

__getTopFans__
getTopFans ∷ Either (Artist, Track) Mbid → Maybe Autocorrect → APIKey → Lastfm Response

__getTopTags__
getTopTags ∷ Either (Artist, Track) Mbid → Maybe Autocorrect → APIKey → Lastfm Response

__love__
love ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

__removeTag__
removeTag ∷ Artist → Track → Tag → APIKey → SessionKey → Secret → Lastfm Response

__scrobble__
scrobble ∷ ( Timestamp, Maybe Album, Artist, Track, Maybe AlbumArtist
           , Maybe Duration, Maybe StreamId, Maybe ChosenByUser
           , Maybe Context, Maybe TrackNumber, Maybe Mbid )
         → APIKey
         → SessionKey
         → Secret
         → Lastfm Response

__search__
search ∷ Track → Maybe Page → Maybe Limit → Maybe Artist → APIKey → Lastfm Response

__share__
share ∷ Artist → Track → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response

__unban__
unban ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

__unlove__
unlove ∷ Artist → Track → APIKey → SessionKey → Secret → Lastfm Response

__updateNowPlaying__
updateNowPlaying ∷ Artist
                 → Track
                 → Maybe Album
                 → Maybe AlbumArtist
                 → Maybe Context
                 → Maybe TrackNumber
                 → Maybe Mbid
                 → Maybe Duration
                 → APIKey
                 → SessionKey
                 → Secret
                 → Lastfm Response
