module Network.Lastfm.API.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

import Network.Lastfm.Internal

getHypedArtists ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getHypedArtists = get "getHypedArtists"

getHypedTracks ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getHypedTracks = get "getHypedTracks"

getLovedTracks ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getLovedTracks = get "getLovedTracks"

getTopArtists ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopArtists = get "getTopArtists"

getTopTags ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTags = get "getTopTags"

getTopTracks ∷ ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks = get "getTopTracks"

get ∷ String → ResponseType → Maybe Page → Maybe Limit → APIKey → Lastfm Response
get method t page limit apiKey = callAPI t
  [ (#) (Method $ "chart." ++ method)
  , (#) page
  , (#) limit
  , (#) apiKey
  ]
