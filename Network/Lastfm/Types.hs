{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
module Network.Lastfm.Types where

import Data.List (intercalate)

newtype Album = Album String deriving (Show, LastfmValue)
newtype AlbumArtist = AlbumArtist String deriving (Show, LastfmValue)
newtype APIKey = APIKey String deriving (Show, LastfmValue)
newtype Artist = Artist String deriving (Show, LastfmValue)
newtype Autocorrect = Autocorrect Bool deriving (Show, LastfmValue)
newtype ChosenByUser = ChosenByUser String deriving (Show, LastfmValue)
newtype Context = Context String deriving (Show, LastfmValue)
newtype Country = Country String deriving (Show, LastfmValue)
newtype Description = Description String deriving (Show, LastfmValue)
newtype Duration = Duration String deriving (Show, LastfmValue)
newtype FestivalsOnly = FestivalsOnly Bool deriving (Show, LastfmValue)
newtype Fingerprint = Fingerprint String deriving (Show, LastfmValue)
newtype From = From Int deriving (Show, LastfmValue)
newtype Group = Group String deriving (Show, LastfmValue)
newtype Language = Language String deriving (Show, LastfmValue)
newtype Limit = Limit Int deriving (Show, LastfmValue)
newtype Mbid = Mbid String deriving (Show, LastfmValue)
newtype Message = Message String deriving (Show, LastfmValue)
newtype Page = Page Int deriving (Show, LastfmValue)
data Period = Week | Quater | HalfYear | Year | Overall deriving (Show)
newtype Playlist = Playlist String deriving (Show, LastfmValue)
newtype Public = Public Bool deriving (Show, LastfmValue)
newtype RecentTracks = RecentTracks Bool deriving (Show, LastfmValue)
newtype Recipient = Recipient String deriving (Show, LastfmValue)
newtype SessionKey = SessionKey String deriving (Show, LastfmValue)
newtype StreamId = StreamId String deriving (Show, LastfmValue)
newtype Tag = Tag String deriving (Show, LastfmValue)
newtype Timestamp = Timestamp String deriving (Show, LastfmValue)
newtype Title = Title String deriving (Show, LastfmValue)
newtype To = To Int deriving (Show, LastfmValue)
newtype Token = Token String deriving (Show, LastfmValue)
newtype Track = Track String deriving (Show, LastfmValue)
newtype TrackNumber = TrackNumber String deriving (Show, LastfmValue)
newtype User = User String deriving (Show, LastfmValue)
newtype UseRecs = UseRecs Bool deriving (Show, LastfmValue)
newtype Venue = Venue String deriving (Show, LastfmValue)

class LastfmValue a where
  unpack :: a -> String

instance LastfmValue Bool where
  unpack True = "1"
  unpack False = "0"
instance LastfmValue Int where
  unpack = show
instance LastfmValue String where
  unpack = id
instance LastfmValue a => LastfmValue [a] where
  unpack = intercalate "," . map unpack
instance LastfmValue a => LastfmValue (Maybe a) where
  unpack (Just a) = unpack a
  unpack Nothing  = ""

instance LastfmValue Period where
  unpack Week     = "7day"
  unpack Quater   = "3month"
  unpack HalfYear = "6month"
  unpack Year     = "12month"
  unpack Overall  = "overall"

(?<) :: LastfmValue a => String -> a -> (String, String)
a ?< b = (a, unpack b)

