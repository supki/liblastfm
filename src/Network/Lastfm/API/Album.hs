module Network.Lastfm.API.Album
  ( addTags, getBuyLinks, getInfo, getShouts, getTags
  , getTopTags, removeTag, search, share
  ) where

import Control.Arrow ((|||))
import Network.Lastfm.Internal

addTags ∷ Format → (Artist, Album) → [Tag] → APIKey → SessionKey → Secret → Lastfm Response
addTags t (artist, album) tags apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "album.addTags")
  , (#) artist
  , (#) album
  , (#) tags
  , (#) apiKey
  , (#) sessionKey
  ]

getBuyLinks ∷ Format → Either (Artist, Album) Mbid → Maybe Autocorrect → Country → APIKey → Lastfm Response
getBuyLinks t a autocorrect country apiKey = callAPI t $
  target a ++
  [ (#) (Method "album.getBuyLinks")
  , (#) autocorrect
  , (#) country
  , (#) apiKey
  ]

getInfo ∷ Format → Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Language → Maybe Username → APIKey → Lastfm Response
getInfo t a autocorrect lang username apiKey = callAPI t $
  target a ++
  [ (#) (Method "album.getInfo")
  , (#) autocorrect
  , (#) lang
  , (#) username
  , (#) apiKey
  ]

getShouts ∷ Format → Either (Artist, Album) Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getShouts t a autocorrect page limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "album.getShouts")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getTags ∷ Format → Either (Artist, Album) Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response
getTags t a autocorrect b apiKey = case b of
  Left user → callAPI t $ target a ++ [(#) user] ++ args
  Right (sessionKey, secret) → callAPIsigned t secret $ target a ++ [(#) sessionKey] ++ args
  where args =
          [ (#) (Method "album.getTags")
          , (#) autocorrect
          , (#) apiKey
          ]

getTopTags ∷ Format → Either (Artist, Album) Mbid → Maybe Autocorrect → APIKey → Lastfm Response
getTopTags t a autocorrect apiKey = callAPI t $
  target a ++
  [ (#) (Method "album.getTopTags")
  , (#) autocorrect
  , (#) apiKey
  ]

removeTag ∷ Format → Artist → Album → Tag → APIKey → SessionKey → Secret → Lastfm Response
removeTag t artist album tag apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "album.removeTag")
  , (#) artist
  , (#) album
  , (#) tag
  , (#) apiKey
  , (#) sessionKey
  ]

search ∷ Format → Album → Maybe Page → Maybe Limit → APIKey → Lastfm Response
search t album page limit apiKey = callAPI t
  [ (#) (Method "album.search")
  , (#) album
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

share ∷ Format → Artist → Album → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response
share t artist album recipient message public apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "album.share")
  , (#) artist
  , (#) album
  , (#) public
  , (#) message
  , (#) recipient
  , (#) apiKey
  , (#) sessionKey
  ]

target ∷ Either (Artist, Album) Mbid → [(String, String)]
target = (\(artist, album) → [(#) artist, (#) album]) ||| return . (#)
