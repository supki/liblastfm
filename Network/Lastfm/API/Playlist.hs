module Network.Lastfm.API.Playlist
  ( addTrack, create
  ) where

import Network.Lastfm.Response
import Network.Lastfm.Types ((?<), APIKey, Artist, Playlist, SessionKey, Title, Description, Track)

addTrack :: Playlist -> Track -> Artist -> APIKey -> SessionKey -> Lastfm ()
addTrack playlist track artist apiKey sessionKey = dispatch $ callAPI_ "playlist.addTrack"
  [ "playlistID" ?< playlist
  , "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

create :: Maybe Title -> Maybe Description -> APIKey -> SessionKey -> Lastfm Response
create title description apiKey sessionKey = dispatch $ callAPI "playlist.create"
  [ "api_key" ?< apiKey
  , "sk" ?< sessionKey
  , "title" ?< title
  , "description" ?< description
  ]
