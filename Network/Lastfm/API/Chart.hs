module Network.Lastfm.API.Chart
  ( getHypedArtists, getHypedTracks, getLovedTracks
  , getTopArtists, getTopTags, getTopTracks
  ) where

import Network.Lastfm.Response
import Network.Lastfm.Types ((?<), APIKey, Limit, Page)

getHypedArtists :: Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getHypedArtists = get "getHypedArtists"

getHypedTracks :: Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getHypedTracks = get "getHypedTracks"

getLovedTracks :: Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getLovedTracks = get "getLovedTracks"

getTopArtists :: Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopArtists = get "getTopArtists"

getTopTags :: Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTags = get "getTopTags"

getTopTracks :: Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks = get "getTopTracks"

get :: String -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
get method page limit apiKey = dispatch $ callAPI ("chart." ++ method)
  [ "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]
