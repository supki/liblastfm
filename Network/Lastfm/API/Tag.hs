module Network.Lastfm.API.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

import Control.Exception (throw)
import Data.Maybe (isJust)
import Prelude hiding (either)

import Network.Lastfm.Response
import Network.Lastfm.Types ((?<), APIKey, Artist, From, Language, Limit, Mbid, Page, Tag, To)

getInfo :: Tag -> Maybe Language -> APIKey -> Lastfm Response
getInfo tag language apiKey = dispatch $ callAPI "tag.getInfo"
  [ "tag" ?< tag
  , "lang" ?< language
  , "api_key" ?< apiKey
  ]

getSimilar :: Tag -> APIKey -> Lastfm Response
getSimilar tag apiKey = dispatch $ callAPI "tag.getSimilar"
  [ "tag" ?< tag
  , "api_key" ?< apiKey
  ]

getTopAlbums :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopAlbums tag page limit apiKey = dispatch $ callAPI "tag.getTopAlbums"
  [ "tag" ?< tag
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getTopArtists :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopArtists tag limit page apiKey = dispatch $ callAPI "tag.getTopArtists"
  [ "tag" ?< tag
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getTopTags :: APIKey -> Lastfm Response
getTopTags apiKey = dispatch $ callAPI "tag.getTopArtists" ["api_key" ?< apiKey]

getTopTracks :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
getTopTracks tag limit page apiKey = dispatch $ callAPI "tag.getTopTracks"
  [ "tag" ?< tag
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getWeeklyArtistChart :: Tag -> Maybe From -> Maybe To -> Maybe Limit -> APIKey -> Lastfm Response
getWeeklyArtistChart tag from to limit apiKey = dispatch $ callAPI "tag.getWeeklyArtistChart"
  [ "tag" ?< tag
  , "from" ?< from
  , "to" ?< to
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

getWeeklyChartList :: Tag -> APIKey -> Lastfm Response
getWeeklyChartList tag apiKey = dispatch $ callAPI "tag.getWeeklyChartList"
  [ "tag" ?< tag
  , "api_key" ?< apiKey
  ]

search :: Tag -> Maybe Page -> Maybe Limit -> APIKey -> Lastfm Response
search tag page limit apiKey = dispatch $ callAPI "tag.search"
  [ "tag" ?< tag
  , "page" ?< page
  , "limit" ?< limit
  , "api_key" ?< apiKey
  ]

either :: String -> Maybe Artist -> Maybe Mbid -> [(String, String)]
either method artist mbid
  | isJust mbid = [ "mbid" ?< mbid ]
  | otherwise   = case artist of
                    Just a  -> [ "artist" ?< a ]
                    Nothing -> throw $ WrapperCallError method "no mbid nor artist are specified."
