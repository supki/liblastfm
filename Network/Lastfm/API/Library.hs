module Network.Lastfm.API.Library
  ( addAlbum, addArtist, addTrack, getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

import Network.Lastfm.Core
import Network.Lastfm.Types ((?<), Album, APIKey, Artist, Limit, Page, SessionKey, Timestamp, Track, User)

addAlbum :: Artist -> Album -> APIKey -> SessionKey -> Lastfm ()
addAlbum artist album apiKey sessionKey = dispatch $ callAPI_ "library.addAlbum"
  [ "artist[1]" ?< artist
  , "album[1]" ?< album
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

addArtist :: Artist -> APIKey -> SessionKey -> Lastfm ()
addArtist artist apiKey sessionKey = dispatch $ callAPI_ "library.addArtist"
  [ "artist[1]" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

addTrack :: Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
addTrack artist track apiKey sessionKey = dispatch $ callAPI_ "library.addTrack"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

getAlbums :: User -> Maybe Artist -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getAlbums user artist limit page apiKey = dispatch $ callAPI "library.getAlbums"
  [ "user" ?< user
  , "artist" ?< artist
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getArtists :: User -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getArtists user limit page apiKey = dispatch $ callAPI "library.getArtists"
  [ "user" ?< user
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

getTracks :: User -> Maybe Artist -> Maybe Album -> Maybe Limit -> Maybe Page -> APIKey -> Lastfm Response
getTracks user artist album limit page apiKey = dispatch $ callAPI "library.getTracks"
  [ "user" ?< user
  , "artist" ?< artist
  , "album" ?< album
  , "limit" ?< limit
  , "page" ?< page
  , "api_key" ?< apiKey
  ]

removeAlbum :: Artist -> Album -> APIKey -> SessionKey -> Lastfm ()
removeAlbum artist album apiKey sessionKey = dispatch $ callAPI_ "library.removeAlbum"
  [ "artist" ?< artist
  , "album" ?< album
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

removeArtist :: Artist -> APIKey -> SessionKey -> Lastfm ()
removeArtist artist apiKey sessionKey = dispatch $ callAPI_ "library.removeArtist"
  [ "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

removeScrobble :: Artist -> Track -> Timestamp -> APIKey -> SessionKey -> Lastfm ()
removeScrobble artist track timestamp apiKey sessionKey = dispatch $ callAPI_ "library.removeScrobble"
  [ "artist" ?< artist
  , "track" ?< track
  , "timestamp" ?< timestamp
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

removeTrack :: Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
removeTrack artist track apiKey sessionKey = dispatch $ callAPI_ "library.removeTrack"
  [ "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
