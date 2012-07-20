module Network.Lastfm.API.Track
  ( addTags, ban, getBuyLinks, getCorrection, getFingerprintMetadata
  , getInfo, getShouts, getSimilar, getTags, getTopFans, getTopTags
  , love, removeTag, scrobble, search, share, unban, unlove, updateNowPlaying
  ) where

import Control.Arrow ((|||))
import Network.Lastfm.Internal

addTags ∷ Format → Artist → Track → [Tag] → APIKey → SessionKey → Secret → Lastfm Response
addTags t artist track tags apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.addTags")
  , (#) artist
  , (#) track
  , (#) tags
  , (#) apiKey
  , (#) sessionKey
  ]

ban ∷ Format → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
ban t artist track apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.ban")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

getBuyLinks ∷ Format → Either (Artist, Track) Mbid → Maybe Autocorrect → Country → APIKey → Lastfm Response
getBuyLinks t a autocorrect country apiKey = callAPI t $
  target a ++
  [ (#) (Method "track.getBuyLinks")
  , (#) autocorrect
  , (#) country
  , (#) apiKey
  ]

getCorrection ∷ Format → Artist → Track → APIKey → Lastfm Response
getCorrection t artist track apiKey = callAPI t
  [ (#) (Method "track.getCorrection")
  , (#) artist
  , (#) track
  , (#) apiKey
  ]

getFingerprintMetadata ∷ Format → Fingerprint → APIKey → Lastfm Response
getFingerprintMetadata t fingerprint apiKey = callAPI t
  [ (#) (Method "track.getFingerprintMetadata")
  , (#) fingerprint
  , (#) apiKey
  ]

getInfo ∷ Format → Either (Artist, Track) Mbid → Maybe Autocorrect → Maybe Username → APIKey → Lastfm Response
getInfo t a autocorrect username apiKey = callAPI t $
  target a ++
  [ (#) (Method "track.getInfo")
  , (#) autocorrect
  , (#) username
  , (#) apiKey
  ]

getShouts ∷ Format → Either (Artist, Track) Mbid → Maybe Autocorrect → Maybe Page → Maybe Limit → APIKey → Lastfm Response
getShouts t a autocorrect page limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "track.getShouts")
  , (#) autocorrect
  , (#) page
  , (#) limit
  , (#) apiKey
  ]

getSimilar ∷ Format → Either (Artist, Track) Mbid → Maybe Autocorrect → Maybe Limit → APIKey → Lastfm Response
getSimilar t a autocorrect limit apiKey = callAPI t $
  target a ++
  [ (#) (Method "track.getSimilar")
  , (#) autocorrect
  , (#) limit
  , (#) apiKey
  ]

getTags ∷ Format → Either (Artist, Track) Mbid → Maybe Autocorrect → Either User (SessionKey, Secret) → APIKey → Lastfm Response
getTags t a autocorrect b apiKey = case b of
  Left user → callAPI t $ target a ++ [(#) user] ++ args
  Right (sessionKey, secret) → callAPIsigned t secret $ target a ++ [(#) sessionKey] ++ args
  where args =
          [ (#) (Method "track.getTags")
          , (#) autocorrect
          , (#) apiKey
          ]

getTopFans ∷ Format → Either (Artist, Track) Mbid → Maybe Autocorrect → APIKey → Lastfm Response
getTopFans t a autocorrect apiKey = callAPI t $
  target a ++
  [ (#) (Method "track.getTopFans")
  , (#) autocorrect
  , (#) apiKey
  ]

getTopTags ∷ Format → Either (Artist, Track) Mbid → Maybe Autocorrect → APIKey → Lastfm Response
getTopTags t a autocorrect apiKey = callAPI t $
  target a ++
  [ (#) (Method "track.getTopTags")
  , (#) autocorrect
  , (#) apiKey
  ]

love ∷ Format → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
love t artist track apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.love")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

removeTag ∷ Format → Artist → Track → Tag → APIKey → SessionKey → Secret → Lastfm Response
removeTag t artist track tag apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.removeTag")
  , (#) artist
  , (#) track
  , (#) tag
  , (#) apiKey
  , (#) sessionKey
  ]

scrobble ∷ Format → ( Timestamp, Maybe Album, Artist, Track, Maybe AlbumArtist
           , Maybe Duration, Maybe StreamId, Maybe ChosenByUser
           , Maybe Context, Maybe TrackNumber, Maybe Mbid )
         → APIKey
         → SessionKey
         → Secret
         → Lastfm Response
scrobble t (timestamp, album, artist, track, albumArtist, duration, streamId, chosenByUser, context, trackNumber, mbid) apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.scrobble")
  , (#) timestamp
  , (#) artist
  , (#) track
  , (#) album
  , (#) albumArtist
  , (#) duration
  , (#) streamId
  , (#) chosenByUser
  , (#) context
  , (#) trackNumber
  , (#) mbid
  , (#) apiKey
  , (#) sessionKey
  ]

search ∷ Format → Track → Maybe Page → Maybe Limit → Maybe Artist → APIKey → Lastfm Response
search t track page limit artist apiKey = callAPI t
  [ (#) (Method "track.search")
  , (#) track
  , (#) page
  , (#) limit
  , (#) artist
  , (#) apiKey
  ]

share ∷ Format → Artist → Track → Recipient → Maybe Message → Maybe Public → APIKey → SessionKey → Secret → Lastfm Response
share t artist track recipient message public apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.share")
  , (#) artist
  , (#) track
  , (#) recipient
  , (#) public
  , (#) message
  , (#) apiKey
  , (#) sessionKey
  ]

unban ∷ Format → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
unban t artist track apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.unban")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]

unlove ∷ Format → Artist → Track → APIKey → SessionKey → Secret → Lastfm Response
unlove t artist track apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.unlove")
  , (#) artist
  , (#) track
  , (#) apiKey
  , (#) sessionKey
  ]


updateNowPlaying ∷ Format → Artist
                 → Track
                 → Maybe Album
                 → Maybe AlbumArtist
                 → Maybe Context
                 → Maybe TrackNumber
                 → Maybe Mbid
                 → Maybe Duration
                 → APIKey
                 → SessionKey
                 → Secret
                 → Lastfm Response
updateNowPlaying t artist track album albumArtist context trackNumber mbid duration apiKey sessionKey secret = callAPIsigned t secret
  [ (#) (Method "track.updateNowPlaying")
  , (#) artist
  , (#) track
  , (#) album
  , (#) albumArtist
  , (#) context
  , (#) trackNumber
  , (#) mbid
  , (#) duration
  , (#) apiKey
  , (#) sessionKey
  ]

target ∷ Either (Artist, Track) Mbid → [(String, String)]
target = (\(artist, track) → [(#) artist, (#) track]) ||| return . (#)
