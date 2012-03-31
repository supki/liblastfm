module ETrack (common, auth) where

import Control.Applicative ((<$>))
import Control.Arrow ((|||))
import Control.Monad ((<=<))
import Data.Time (formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale)

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Track as Track

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

addTags :: APIKey -> SessionKey -> Secret -> IO ()
addTags ak sk s = Track.addTags (Artist "Jefferson Airplane") (Track "White rabbit") [Tag "60s", Tag "awesome"] ak sk s >>= print ||| const (return ())

ban :: APIKey -> SessionKey -> Secret -> IO ()
ban ak sk s = Track.ban (Artist "Eminem") (Track "Kim") ak sk s >>= print ||| const (return ())

getBuylinks :: IO ()
getBuylinks = parse r f "Download suppliers"
  where r = Track.getBuyLinks (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Country "United Kingdom") apiKey
        f = mapM (content <=< tag "supplierName") <=< tags "affiliation" <=< tag "downloads" <=< tag "affiliations"

getCorrection :: IO ()
getCorrection = parse r f "Correction"
  where r = Track.getCorrection (Artist "Pink Ployd") (Track "Brain Damage") apiKey
        f = fmap return . content <=< tag "name" <=< tag "artist" <=< tag "track" <=< tag "correction" <=< tag "corrections"

getFingerprintMetadata :: IO ()
getFingerprintMetadata = parse r f "Tracks"
  where r = Track.getFingerprintMetadata (Fingerprint 1234) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "tracks"

getInfo :: IO ()
getInfo = parse r f "Replays count"
  where r = Track.getInfo (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Just $ Username "aswalrus") apiKey
        f = fmap return . content <=< tag "userplaycount" <=< tag "track"

getShouts :: IO ()
getShouts = parse r f "Last 7 shouts authors"
  where r = Track.getShouts (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing Nothing (Just $ Limit 7) apiKey
        f = mapM (content <=< tag "author") <=< tags "shout" <=< tag "shouts"

getSimilar :: IO ()
getSimilar = parse r f "4 similar tracks"
  where r = Track.getSimilar (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing (Just $ Limit 4) apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "similartracks"

getTags :: IO ()
getTags = parse r f "White Rabbit tags"
  where r = Track.getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Left $ User "liblastfm") apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "tags"

getTagsAuth :: APIKey -> SessionKey -> Secret -> IO ()
getTagsAuth ak sk s = parse r f "White Rabbit tags"
  where r = Track.getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Right (sk, s)) ak
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "tags"

getTopFans :: IO ()
getTopFans = parse r f "Top fans"
  where r = Track.getTopFans (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "user" <=< tag "topfans"

getTopTags :: IO ()
getTopTags = parse r f "Top tags"
  where r = Track.getTopTags (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "toptags"

love :: APIKey -> SessionKey -> Secret -> IO ()
love ak sk s = Track.love (Artist "Gojira") (Track "Ocean") ak sk s >>= print ||| const (return ())

removeTag :: APIKey -> SessionKey -> Secret -> IO ()
removeTag ak sk s = Track.removeTag (Artist "Jefferson Airplane") (Track "White rabbit") (Tag "awesome") ak sk s >>= print ||| const (return ())

search :: IO ()
search = parse r f "12 search results for \"Believe\" query"
  where r = Track.search (Track "Believe") Nothing (Just $ Limit 12) Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "trackmatches" <=< tag "results"

share :: APIKey -> SessionKey -> Secret -> IO ()
share ak sk s = Track.share (Artist "Led Zeppelin") (Track "When the Levee Breaks") (Recipient "liblastfm") (Just $ Message "Just listen!") Nothing ak sk s >>= print ||| const (return ())

unban :: APIKey -> SessionKey -> Secret -> IO ()
unban ak sk s = Track.unban (Artist "Eminem") (Track "Kim") ak sk s >>= print ||| const (return ())

unlove :: APIKey -> SessionKey -> Secret -> IO ()
unlove ak sk s = Track.unlove (Artist "Gojira") (Track "Ocean") ak sk s >>= print ||| const (return ())

scrobble :: APIKey -> SessionKey -> Secret -> IO ()
scrobble ak sk s = do
  t <- read . formatTime defaultTimeLocale "%s" <$> getCurrentTime
  r <- Track.scrobble (Timestamp t, Nothing, Artist "Gojira", Track "Ocean", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ak sk s 
  case r of
    Left e -> print e
    Right _ -> return ()

updateNowPlaying :: APIKey -> SessionKey -> Secret -> IO ()
updateNowPlaying ak sk s = Track.updateNowPlaying (Artist "Gojira") (Track "Ocean") Nothing Nothing Nothing Nothing Nothing Nothing ak sk s >>= print ||| const (return ())

common :: IO ()
common = do getBuylinks
            getCorrection
            getFingerprintMetadata
            getInfo
            getShouts
            getSimilar
            getTags
            getTopFans
            getTopTags
            search

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth ak sk s = do addTags ak sk s
                  getTagsAuth ak sk s
                  ban ak sk s
                  love ak sk s
                  removeTag ak sk s
                  scrobble ak sk s
                  share ak sk s
                  unban ak sk s
                  unlove ak sk s
                  updateNowPlaying ak sk s
