module Network.Lastfm.API.Tag
  ( getInfo, getSimilar, getTopAlbums, getTopArtists, getTopTags, getTopTracks
  , getWeeklyArtistChart, getWeeklyChartList, search
  ) where

import Control.Exception (throw)
import Data.Maybe (isJust)
import Prelude hiding (either)

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), APIKey, Artist, From, Language, Limit, Mbid, Page, Tag, To)

getInfo :: Maybe Artist -> Maybe Mbid -> Maybe Language -> APIKey -> Lastfm Response
getInfo artist mbid language apiKey = dispatch $ callAPI method $ parameters ++
  [ "artist" ?< artist
  , "lang" ?< language
  , "api_key" ?< apiKey
  ]
  where method = "tag.getInfo"
        parameters = either method artist mbid

getSimilar :: Tag -> APIKey -> Lastfm Response
getSimilar tag apiKey = dispatch $ callAPI "tag.getSimilar"
  [ "tag" ?< tag
  , "api_key" ?< apiKey
  ]

getTopAlbums :: Tag -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getTopAlbums tag limit page apiKey = dispatch $ callAPI "tag.getTopAlbums"
  [ "tag" ?< tag
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getTopArtists :: Tag -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getTopArtists tag limit page apiKey = dispatch $ callAPI "tag.getTopArtists"
  [ "tag" ?< tag
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getTopTags :: APIKey -> Lastfm Response
getTopTags apiKey = dispatch $ callAPI "tag.getTopArtists" [ "api_key" ?< apiKey ]

getTopTracks :: Tag -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getTopTracks tag limit page apiKey = dispatch $ callAPI "tag.getTopTracks"
  [ "tag" ?< tag
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getWeeklyArtistChart :: Tag -> Maybe From -> Maybe To -> Maybe Limit -> APIKey -> Lastfm Response
getWeeklyArtistChart tag from to limit apiKey = dispatch $ callAPI "tag.getWeeklyArtistsChart"
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

search :: Maybe Limit -> Maybe Page -> Tag -> APIKey -> Lastfm Response
search limit page tag apiKey = dispatch $ callAPI "tag.search"
  [ "limit" ?< limit
  , "page" ?< page
  , "tag" ?< tag
  , "api_key" ?< apiKey
  ]

either :: String -> Maybe Artist -> Maybe Mbid -> [(String, String)]
either method artist mbid
  | isJust mbid = [ "mbid" ?< mbid ]
  | otherwise   = case artist of
                    Just a  -> [ "artist" ?< a ]
                    Nothing -> throw $ WrapperCallError method "no mbid nor artist are specified."
