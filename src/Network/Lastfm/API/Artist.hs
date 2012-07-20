module Network.Lastfm.API.Artist
  ( addTags, getCorrection, getEvents, getImages, getInfo
  , getPastEvents, getPodcast, getShouts, getSimilar, getTags, getTopAlbums
  , getTopFans, getTopTags, getTopTracks, removeTag, search, share, shout
  ) where

import Control.Arrow ((|||))
import Network.Lastfm.Internal

addTags ∷ Format → Artist → [Tag] → APIKey → SessionKey → Secret → Lastfm Response
addTags t artist tags apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "artist.addTags")
  , (#) artist
  , (#) tags
  , (#) apiKey
  , (#) sessionKey
  ]

getCorrection ∷ Format → Artist → APIKey → Lastfm Response
getCorrection t artist apiKey = callAPI t
  [ (#) (Method "artist.getCorrection")
  , (#) artist
  , (#) apiKey
  ]

getEvents ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → Maybe FestivalsOnly → APIKey → Lastfm Response
getEvents t a autocorrect page limit festivalsOnly apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getEvents")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) festivalsOnly
  , (#) apiKey
  ]

getImages ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → Maybe Order → APIKey → Lastfm Response
getImages t a autocorrect page limit order apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getImages")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) order
  , (#) apiKey
  ]

getInfo ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Language → Maybe Username → APIKey → Lastfm Response
getInfo t a autocorrect language username apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getInfo")
  , (#) autocorrect
  , (#) language
  , (#) username
  , (#) apiKey
  ]

getPastEvents ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getPastEvents t a autocorrect page limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getPastEvents")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getPodcast ∷ Format → Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response
getPodcast t a autocorrect apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getPodcast")
  , (#) autocorrect
  , (#) apiKey
  ]

getShouts ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getShouts t a autocorrect page limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getShouts")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getSimilar ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Limit → APIKey → Lastfm Response
getSimilar t a autocorrect limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getSimilar")
  , (#) autocorrect
  , (#) limit
  , (#) apiKey
  ]

getTags ∷ Format → Either Artist Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response
getTags t a autocorrect b apiKey = case b of
  Left user → callAPI t $ target a ++ [(#) user] ++ args
  Right (sessionKey, secret) → callAPIsigned t secret $ target a ++ [(#) sessionKey] ++ args
  where args =
          [ (#) (Method "artist.getTags")
          , (#) autocorrect
          , (#) apiKey
          ]

getTopAlbums ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopAlbums t a autocorrect page limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getTopAlbums")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTopFans ∷ Format → Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response
getTopFans t a autocorrect apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getTopFans")
  , (#) autocorrect
  , (#) apiKey
  ]

getTopTags ∷ Format → Either Artist Mbid → Maybe Autocorrect → APIKey → Lastfm Response
getTopTags t a autocorrect apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getTopTags")
  , (#) autocorrect
  , (#) apiKey
  ]

getTopTracks ∷ Format → Either Artist Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getTopTracks t a autocorrect page limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "artist.getTopTracks")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

removeTag ∷ Format → Artist → Tag → APIKey → SessionKey → Secret → Lastfm Response
removeTag t artist tag apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "artist.removeTag")
  , (#) artist
  , (#) tag
  , (#) apiKey
  , (#) sessionKey
  ]

search ∷ Format → Artist → Maybe Page → Maybe Limit → APIKey → Lastfm Response
search t artist page limit apiKey = callAPI t
  [ (#) (Method "artist.search")
  , (#) artist
  , (#) apiKey
  , (#) page
  , (#) limit
  ]

share ∷ Format → Artist → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response
share t artist recipient message public apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "artist.share")
  , (#) artist
  , (#) recipient
  , (#) apiKey
  , (#) sessionKey
  , (#) public
  , (#) message
  ]

shout ∷ Format → Artist → Message → APIKey → SessionKey → Secret → Lastfm Response
shout t artist message apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "artist.shout")
  , (#) artist
  , (#) message
  , (#) apiKey
  , (#) sessionKey
  ]

target ∷ Either Artist Mbid → [(String, String)]
target = return . (#) ||| return . (#)
