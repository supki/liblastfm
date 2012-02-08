module Network.Lastfm.API.Playlist
  ( addTrack, create
  ) where

import Network.Lastfm.Response
import Network.Lastfm.Types ((?<), APIKey, Artist, Playlist, SessionKey, Title, Description, Track)

addTrack :: Playlist -> Artist -> Track -> APIKey -> SessionKey -> Lastfm ()
addTrack playlist artist track apiKey sessionKey = dispatch $ callAPI_ "playlist.addTrack"
  [ "playlistID" ?< playlist
  , "artist" ?< artist
  , "track" ?< track
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

create :: Maybe Title -> Maybe Description -> APIKey -> SessionKey -> Lastfm ()
create title description apiKey sessionKey = dispatch $ callAPI_ "playlist.create"
  [ "api_key" ?< apiKey
  , "sk" ?< sessionKey
  , "title" ?< title
  , "description" ?< description
  ]
