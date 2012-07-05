{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Album API module
{-# OPTIONS_HADDOCK prune #-}
module Network.Lastfm.JSON.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

#include "album.docs"

import Network.Lastfm.Internal
import qualified Network.Lastfm.API.Album as API

$(json ["addTags", "getBuyLinks", "getInfo", "getShouts", "getTags", "getTopTags", "removeTag", "search", "share"])

__addTags__
addTags ∷ (Artist, Album) → [Tag] → APIKey → SessionKey → Secret → Lastfm Response

__getBuyLinks__
getBuyLinks ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Country → APIKey → Lastfm Response

__getInfo__
getInfo ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Language → Maybe Username → APIKey → Lastfm Response

__getShouts__
getShouts ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__getTags__
getTags ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response

__getTopTags__
getTopTags ∷ Either (Artist, Album) Mbid → Maybe Autocorrect → APIKey → Lastfm Response

__removeTag__
removeTag ∷ Artist → Album → Tag → APIKey → SessionKey → Secret → Lastfm Response

__search__
search ∷ Album → Maybe Page → Maybe Limit → APIKey → Lastfm Response

__share__
share ∷ Artist → Album → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response
