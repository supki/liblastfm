module Network.Lastfm.API.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

import Network.Lastfm

getHypedArtists = get "getHypedArtists"

getHypedTracks = get "getHypedTracks"

getLovedTracks = get "getLovedTracks"

getTopArtists = get "getTopArtists"

getTopTags = get "getTopTags"

getTopTracks = get "getTopTracks"

get ∷ String → ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
get method t page limit apiKey = callAPI t
  [ (#) (Method $ "chart." ++ method)
  , (#) page
  , (#) limit
  , (#) apiKey
  ]
