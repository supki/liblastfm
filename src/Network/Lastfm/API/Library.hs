module Network.Lastfm.API.Library
  ( addAlbum, addArtist, addTrack, getAlbums, getArtists, getTracks
  , removeAlbum, removeArtist, removeScrobble, removeTrack
  ) where

import Network.Lastfm.Internal

addAlbum ∷ ResponseType → Artist → Album → APIKey → SessionKey → Secret → Lastfm Response
addAlbum t artist album apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "library.addAlbum")
  , (#) artist
  , (#) album
  , (#) apiKey
  , (#) sessionKey
  ]

addArtist ∷ ResponseType → Artist → APIKey → SessionKey → Secret → Lastfm Response
addArtist t artist apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "library.addArtist")
  , (#) artist
  , (#) apiKey
  , (#) sessionKey
  ]

addTrack ∷ ResponseType → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
addTrack t artist track apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "library.addTrack")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

getAlbums ∷ ResponseType → User → Maybe Artist → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getAlbums t user artist page limit apiKey = callAPI t
  [ (#) (Method "library.getAlbums")
  , (#) user
  , (#) artist
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getArtists ∷ ResponseType → User → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getArtists t user page limit apiKey = callAPI t
  [ (#) (Method "library.getArtists")
  , (#) user
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTracks ∷ ResponseType → User → Maybe Artist → Maybe Album → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTracks t user artist album page limit apiKey = callAPI t
  [ (#) (Method "library.getTracks")
  , (#) user
  , (#) artist
  , (#) album
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

removeAlbum ∷ ResponseType → Artist → Album → APIKey → SessionKey → Secret → Lastfm Response
removeAlbum t artist album apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "library.removeAlbum")
  , (#) artist
  , (#) album
  , (#) apiKey
  , (#) sessionKey
  ]

removeArtist ∷ ResponseType → Artist → APIKey → SessionKey → Secret → Lastfm Response
removeArtist t artist apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "library.removeArtist")
  , (#) artist
  , (#) apiKey
  , (#) sessionKey
  ]

removeScrobble ∷ ResponseType → Artist → Track → Timestamp → APIKey → SessionKey → Secret → Lastfm Response
removeScrobble t artist track timestamp apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "library.removeScrobble")
  , (#) artist
  , (#) track
  , (#) timestamp
  , (#) apiKey
  , (#) sessionKey
  ]

removeTrack ∷ ResponseType → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
removeTrack t artist track apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "library.removeTrack")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]
