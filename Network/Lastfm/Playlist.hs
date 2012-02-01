{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Playlist
  ( PlaylistID(..), Artist
  , addTrack, create
  ) where

import Network.Lastfm.Artist (Artist)
import Network.Lastfm.Auth (APIKey, SessionKey)
import Network.Lastfm.Core
import Network.Lastfm.Track (Track)

newtype Description = Description String deriving (Show, LastfmValue)
newtype PlaylistID = PlaylistID String deriving (Show, LastfmValue)
newtype Title = Title String deriving (Show, LastfmValue)

addTrack :: PlaylistID -> Track -> Artist -> APIKey -> SessionKey -> IO ()
addTrack playlist track artist apiKey sessionKey = callAPI_ "playlist.addTrack"
  [ "playlistID" ?< playlist
  , "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

create :: Maybe Title -> Maybe Description -> APIKey -> SessionKey -> IO Response
create title description apiKey sessionKey = callAPI "playlist.create" $
  [ "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ] ++ optional
    [ "title" ?<< title
    , "description" ?<< description
    ]
