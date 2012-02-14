module ETrack (common, auth) where

import Control.Arrow ((|||))
import Control.Monad ((<=<))

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Track as Track

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

addTags :: APIKey -> SessionKey -> IO ()
addTags ak sk = Track.addTags (Artist "Jefferson Airplane") (Track "White rabbit") [Tag "60s", Tag "awesome"] ak sk >>= print ||| const (return ())

ban :: APIKey -> SessionKey -> IO ()
ban ak sk = Track.ban (Artist "Eminem") (Track "Kim") ak sk >>= print ||| const (return ())

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
  where r = Track.getInfo (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Just $ User "aswalrus") apiKey
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

getTagsAuth :: APIKey -> SessionKey -> IO ()
getTagsAuth ak sk = parse r f "White Rabbit tags"
  where r = Track.getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Right sk) ak
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "tags"

getTopFans :: IO ()
getTopFans = parse r f "Top fans"
  where r = Track.getTopFans (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "user" <=< tag "topfans"

getTopTags :: IO ()
getTopTags = parse r f "Top tags"
  where r = Track.getTopTags (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "toptags"

love :: APIKey -> SessionKey -> IO ()
love ak sk = Track.love (Artist "Gojira") (Track "Ocean") ak sk >>= print ||| const (return ())

removeTag :: APIKey -> SessionKey -> IO ()
removeTag ak sk = Track.removeTag (Artist "Jefferson Airplane") (Track "White rabbit") (Tag "awesome") ak sk >>= print ||| const (return ())

search :: IO ()
search = parse r f "12 search results for \"Believe\" query"
  where r = Track.search (Track "Believe") Nothing (Just $ Limit 12) Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "track" <=< tag "trackmatches" <=< tag "results"

share :: APIKey -> SessionKey -> IO ()
share ak sk = Track.share (Artist "Led Zeppelin") (Track "When the Levee Breaks") [Recipient "liblastfm"] (Just $ Message "Just listen!") Nothing ak sk >>= print ||| const (return ())

unban :: APIKey -> SessionKey -> IO ()
unban ak sk = Track.unban (Artist "Eminem") (Track "Kim") ak sk >>= print ||| const (return ())

unlove :: APIKey -> SessionKey -> IO ()
unlove ak sk = Track.unlove (Artist "Gojira") (Track "Ocean") ak sk >>= print ||| const (return ())

scrobble :: APIKey -> SessionKey -> IO ()
scrobble ak sk = Track.scrobble (Timestamp 1328905590, Nothing, Artist "Gojira", Track "Ocean", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) ak sk >>= print ||| const (return ())

updateNowPlaying :: APIKey -> SessionKey -> IO ()
updateNowPlaying ak sk = Track.updateNowPlaying (Artist "Gojira") (Track "Ocean") Nothing Nothing Nothing Nothing Nothing Nothing ak sk >>= print ||| const (return ())

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

auth :: APIKey -> SessionKey -> IO ()
auth ak sk = do addTags ak sk
                getTagsAuth ak sk
                ban ak sk
                love ak sk
                removeTag ak sk
                scrobble ak sk
                share ak sk
                unban ak sk
                unlove ak sk
                updateNowPlaying ak sk
