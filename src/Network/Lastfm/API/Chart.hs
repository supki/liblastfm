module Network.Lastfm.API.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

import Network.Lastfm.Internal

getHypedArtists ∷ Format → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getHypedArtists = get "getHypedArtists"

getHypedTracks ∷ Format → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getHypedTracks = get "getHypedTracks"

getLovedTracks ∷ Format → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getLovedTracks = get "getLovedTracks"

getTopArtists ∷ Format → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists = get "getTopArtists"

getTopTags ∷ Format → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTags = get "getTopTags"

getTopTracks ∷ Format → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks = get "getTopTracks"

get ∷ String → Format → Maybe Page → Maybe Limit → APIKey → Lastfm Response
get method t page limit apiKey = callAPI t
  [ (#) (Method $ "chart." ++ method)
  , (#) page
  , (#) limit
  , (#) apiKey
  ]
