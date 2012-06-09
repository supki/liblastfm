module Network.Lastfm.API.Playlist
  ( addTrack, create
  ) where

import Network.Lastfm.Internal

addTrack ∷ ResponseType → Playlist → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
addTrack t playlist artist track apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "playlist.addTrack")
  , (#) playlist
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

create ∷ ResponseType → Maybe Title → Maybe Description → APIKey → SessionKey → Secret → Lastfm Response
create t title description apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "playlist.create")
  , (#) title
  , (#) description
  , (#) apiKey
  , (#) sessionKey
  ]
