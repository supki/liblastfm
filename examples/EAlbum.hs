module EAlbum (common, auth) where

import Control.Arrow ((|||))
import Control.Monad ((<=<))

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Album as Album

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

addTags :: APIKey -> SessionKey -> IO ()
addTags ak sk = Album.addTags (Artist "Pink Floyd", Album "The Wall") [Tag "70s", Tag "awesome"] ak sk >>= print ||| const (return ())

getBuylinks :: IO ()
getBuylinks = parse r f "Download suppliers"
  where r = Album.getBuyLinks (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Country "United Kingdom") apiKey
        f = mapM (content <=< tag "supplierName") <=< tags "affiliation" <=< tag "downloads" <=< tag "affiliations"

getInfo :: IO ()
getInfo = parse r f "Top 5 tags"
  where r = Album.getInfo (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing Nothing apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "toptags" <=< tag "album"

getShouts :: IO ()
getShouts = parse r f "Last 7 shouts"
  where r = Album.getShouts (Left (Artist "Pink Floyd", Album "The Wall")) Nothing Nothing (Just $ Limit 7) apiKey
        f = mapM (content <=< tag "body") <=< tags "shout" <=< tag "shouts"

getTags :: IO ()
getTags = parse r f "The Wall tags"
  where r = Album.getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Left $ User "liblastfm") apiKey
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "tags"

getTagsAuth :: APIKey -> SessionKey -> IO ()
getTagsAuth ak sk = parse r f "The Wall tags"
  where r = Album.getTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing (Right sk) ak
        f = mapM (content <=< tag "name") <=< tags "tag" <=< tag "tags"

getTopTags :: IO ()
getTopTags = parse r f "Top tags counts"
  where r = Album.getTopTags (Left (Artist "Pink Floyd", Album "The Wall")) Nothing apiKey
        f = mapM (content <=< tag "count") <=< tags "tag" <=< tag "toptags"

removeTag :: APIKey -> SessionKey -> IO ()
removeTag ak sk = Album.removeTag (Artist "Pink Floyd") (Album "The Wall") (Tag "awesome") ak sk >>= print ||| const (return ())

search :: IO ()
search = parse r f "5 search results for \"wall\" query"
  where r = Album.search (Album "wall") Nothing (Just (Limit 5)) apiKey
        f = mapM (content <=< tag "name") <=< tags "album" <=< tag "albummatches" <=< tag "results"

share :: APIKey -> SessionKey -> IO ()
share ak sk = Album.share (Artist "Sleep") (Album "Jerusalem") [Recipient "liblastfm"] (Just $ Message "Just listen!") Nothing ak sk >>= print ||| const (return ())

common :: IO ()
common = do getBuylinks
            getInfo
            getShouts
            getTopTags
            search
            getTags

auth :: APIKey -> SessionKey -> IO ()
auth ak sk = do addTags ak sk
                getTagsAuth ak sk
                removeTag ak sk
                share ak sk
