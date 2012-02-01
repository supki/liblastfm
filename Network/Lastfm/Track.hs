{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Track
  ( APIKey, SessionKey
  , Album, AlbumArtist, Artist, ChosenByUser, Context
  , Duration, Limit, Mbid, Message, Page, Public, Recipient
  , StreamId, Tag, Timestamp , Track, TrackNumber
  , ban, unban
  , love, unlove
  , scrobble, updateNowPlaying
  , addTags, removeTag
  , search, share,
  ) where

import Network.Lastfm.Core

newtype APIKey = APIKey String deriving LastfmValue
newtype SessionKey = SessionKey String deriving LastfmValue

newtype Album = Album String deriving LastfmValue
newtype AlbumArtist = AlbumArtist String deriving LastfmValue
newtype Artist = Artist String deriving LastfmValue
newtype ChosenByUser = ChosenByUser String deriving LastfmValue
newtype Context = Context String deriving LastfmValue
newtype Duration = Duration String deriving LastfmValue
newtype Limit = Limit Int deriving LastfmValue
newtype Mbid = Mbid String deriving LastfmValue
newtype Message = Message String deriving LastfmValue
newtype Page = Page String deriving LastfmValue
newtype Public = Public String deriving LastfmValue
newtype Recipient = Recipient String deriving LastfmValue
newtype StreamId = StreamId String deriving LastfmValue
newtype Tag = Tag String deriving LastfmValue
newtype Timestamp = Timestamp String deriving LastfmValue
newtype Track = Track String deriving LastfmValue
newtype TrackNumber = TrackNumber String deriving LastfmValue

ban :: Track -> Artist -> APIKey -> SessionKey -> IO ()
ban track artist apiKey sessionKey = callAPI_ "track.ban"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

unban :: Track -> Artist -> APIKey -> SessionKey -> IO ()
unban track artist apiKey sessionKey = callAPI_ "track.unban"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

love :: Track -> Artist -> APIKey -> SessionKey -> IO ()
love track artist apiKey sessionKey = callAPI_ "track.love"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

unlove :: Track -> Artist -> APIKey -> SessionKey -> IO ()
unlove track artist apiKey sessionKey = callAPI_ "track.unlove"
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]

updateNowPlaying :: Track
                 -> Artist
                 -> Maybe Album
                 -> Maybe AlbumArtist
                 -> Maybe Context
                 -> Maybe TrackNumber
                 -> Maybe Mbid
                 -> Maybe Duration
                 -> APIKey
                 -> SessionKey
                 -> IO ()
updateNowPlaying track artist album albumArtist context trackNumber mbid duration apiKey sessionKey = callAPI_ "track.updateNowPlaying" $
  [ "track" ?< track
  , "artist" ?< artist
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ] ++ optional
    [ ("album" ?<< album)
    , ("albumArtist" ?<< albumArtist)
    , ("context" ?<< context)
    , ("trackNumber" ?<< trackNumber)
    , ("mbid" ?<< mbid)
    , ("duration" ?<< duration)
    ]

scrobble :: [ ( Timestamp, Maybe Album, Track, Artist, Maybe AlbumArtist
           , Maybe Duration, Maybe StreamId, Maybe ChosenByUser
           , Maybe Context, Maybe TrackNumber, Maybe Mbid ) ]
         -> APIKey
         -> SessionKey
         -> IO ()
scrobble xs apiKey sessionKey = mapM_ scrobbleTrack xs
  where scrobbleTrack (timestamp, album, track, artist, albumArtist, duration, streamId, chosenByUser, context, trackNumber, mbid) = callAPI_ "track.scrobble" $
          [ "timestamp" ?< timestamp
          , "track" ?< track
          , "artist" ?< artist
          , "api_key" ?< apiKey
          , "sk" ?< sessionKey
          ] ++ optional
            [ "album" ?<< album
            , "albumArtist" ?<< albumArtist
            , "duration" ?<< duration
            , "streamId" ?<< streamId
            , "chosenByUser" ?<< chosenByUser
            , "context" ?<< context
            , "trackNumber" ?<< trackNumber
            , "mbid" ?<< mbid
            ]

search :: Maybe Limit -> Maybe Page -> Track -> Maybe Artist -> APIKey -> IO Response
search limit page track artist apiKey = callAPI "track.search" $
  [ "track" ?< track
  , "api_key" ?< apiKey
  ] ++ optional
    [ "limit" ?<< limit
    , "page" ?<< page
    , "artist" ?<< artist
    ]

share :: Artist -> Track -> Maybe Public -> Maybe Message -> [Recipient] -> APIKey -> SessionKey -> IO ()
share artist track public message recipients apiKey sessionKey = callAPI_ "track.share" $
  [ "artist" ?< artist
  , "track" ?< track
  , "recipient" ?< recipients
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ] ++ optional
    [ "public" ?<< public
    , "message" ?<< message
    ]

addTags :: Artist -> Track -> [Tag] -> APIKey -> SessionKey -> IO ()
addTags artist track tags apiKey sessionKey
  | null tags        = error "Track.addTags: empty tag list."
  | length tags > 10 = error "Track.addTags: tag list length has exceeded maximum."
  | otherwise        = callAPI_ "track.addTags" $
    [ "artist" ?< artist
    , "track" ?< track
    , "tags" ?< tags
    , "api_key" ?< apiKey
    , "sk" ?< sessionKey
    ]

removeTag :: Artist -> Track -> Tag -> APIKey -> SessionKey -> IO ()
removeTag artist track tag apiKey sessionKey = callAPI_ "track.removeTag" $
  [ "artist" ?< artist
  , "track" ?< track
  , "tag" ?< tag
  , "api_key" ?< apiKey
  , "sk" ?< sessionKey
  ]
