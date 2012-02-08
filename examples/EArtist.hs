module EArtist (start) where

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Artist as Artist

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

addTags :: APIKey -> SessionKey -> IO ()
addTags apiKey sessionKey = do response <- Artist.addTags (Artist "Burzum") [Tag "black metal", Tag "depressive"] apiKey sessionKey
                               case response of
                                 Left e   -> print e
                                 Right () -> return ()

getCorrection :: IO ()
getCorrection = do response <- Artist.getCorrection (Artist "Meshugah") apiKey
                   putStr "Correction: "
                   case response of
                     Left e  -> print e
                     Right r -> print (correction r)
                   putStrLn ""
  where correction = getContent <=< lookupChild "name" <=< lookupChild "artist" <=< lookupChild "correction" <=< lookupChild "corrections" <=< wrap

getEvents :: IO ()
getEvents = do response <- Artist.getEvents (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) Nothing apiKey
               putStr "First event place: "
               case response of
                 Left e  -> print e
                 Right r -> print (place r)
               putStrLn ""
  where place = getContent <=< lookupChild "name" <=< lookupChild "venue" <=< lookupChild "event" <=< lookupChild "events" <=< wrap

getImages :: IO ()
getImages = do response <- Artist.getImages (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) Nothing apiKey
               putStr "First three images links: "
               case response of
                 Left e  -> print e
                 Right r -> print (links r)
               putStrLn ""
  where links = mapM (getContent <=< lookupChild "url") <=< lookupChildren "image" <=< lookupChild "images" <=< wrap

getInfo :: IO ()
getInfo = do response <- Artist.getInfo (Left $ Artist "Meshuggah") Nothing Nothing Nothing apiKey
             putStr "Listeners count: "
             case response of
               Left e  -> print e
               Right r -> print (listeners r)
             putStrLn ""
  where listeners = getContent <=< lookupChild "listeners" <=< lookupChild "stats" <=< lookupChild "artist" <=< wrap

getPastEvents :: IO ()
getPastEvents = do response <- Artist.getPastEvents (Left $ Artist "Meshugah") (Just $ Autocorrect True) Nothing Nothing apiKey
                   putStr "All event artists: "
                   case response of
                     Left e  -> print e
                     Right r -> print (artists r)
                   putStrLn ""
  where artists = mapM getContent <=< lookupChildren "artist" <=< lookupChild "artists" <=< lookupChild "event" <=< lookupChild "events" <=< wrap

getPodcast :: IO ()
getPodcast = do response <- Artist.getPodcast (Left $ Artist "Meshuggah") Nothing apiKey
                putStr "First channel description: "
                case response of
                  Left e  -> print e
                  Right r -> print (description r)
                putStrLn ""
  where description = getContent <=< lookupChild "description" <=< lookupChild "channel" <=< lookupChild "rss" <=< wrap

getShouts :: IO ()
getShouts = do response <- Artist.getShouts (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 5) apiKey
               putStr "Last 5 shouts authors: "
               case response of
                 Left e  -> print e
                 Right r -> print (authors r)
               putStrLn ""
  where authors = mapM (getContent <=< lookupChild "author") <=< lookupChildren "shout" <=< lookupChild "shouts" <=< wrap

getSimilar :: IO ()
getSimilar = do response <- Artist.getSimilar (Left $ Artist "Meshuggah") Nothing (Just $ Limit 7) apiKey
                putStr "7 similar artists: "
                case response of
                  Left e  -> print e
                  Right r -> print (artists r)
                putStrLn ""
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "similarartists" <=< wrap

getTags :: IO ()
getTags = do response <- Artist.getTags (Left $ Artist "Burzum") Nothing (Left $ User "liblastfm") apiKey
             putStr "Burzum tags: "
             case response of
               Left e  -> print e
               Right r -> print (tags r)
             putStrLn ""
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "tags" <=< wrap

getTagsAuth :: APIKey -> SessionKey -> IO ()
getTagsAuth apiKey sessionKey = do response <- Artist.getTags (Left $ Artist "Burzum") Nothing (Right sessionKey) apiKey
                                   putStr "Burzum tags: "
                                   case response of
                                     Left e  -> print e
                                     Right r -> print (tags r)
                                   putStrLn ""
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "tags" <=< wrap

getTopAlbums :: IO ()
getTopAlbums = do response <- Artist.getTopAlbums (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 3) apiKey
                  putStr "3 most popular albums: "
                  case response of
                    Left e  -> print e
                    Right r -> print (albums r)
                  putStrLn ""
  where albums = mapM (getContent <=< lookupChild "name") <=< lookupChildren "album" <=< lookupChild "topalbums" <=< wrap

getTopFans :: IO ()
getTopFans = do response <- Artist.getTopFans (Left $ Artist "Meshuggah") Nothing apiKey
                putStr "Top fans: "
                case response of
                  Left e  -> print e
                  Right r -> print (fans r)
                putStrLn ""
  where fans = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "topfans" <=< wrap

getTopTags :: IO ()
getTopTags = do response <- Artist.getTopTags (Left $ Artist "Meshuggah") Nothing apiKey
                putStr "Top tags: "
                case response of
                  Left e  -> print e
                  Right r -> print (tags r)
                putStrLn ""
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "toptags" <=< wrap

getTopTracks :: IO ()
getTopTracks = do response <- Artist.getTopTracks (Left $ Artist "Meshuggah") Nothing Nothing (Just $ Limit 10) apiKey
                  putStr "10 most popular tracks: "
                  case response of
                    Left e  -> print e
                    Right r -> print (tracks r)
                  putStrLn ""
  where tracks = mapM (getContent <=< lookupChild "name") <=< lookupChildren "track" <=< lookupChild "toptracks" <=< wrap

removeTag :: APIKey -> SessionKey -> IO ()
removeTag apiKey sessionKey = do response <- Artist.removeTag (Artist "Burzum") (Tag "black metal") apiKey sessionKey
                                 case response of
                                   Left e   -> print e
                                   Right () -> return ()

search :: IO ()
search = do response <- Artist.search (Artist "Mesh") Nothing (Just (Limit 12)) apiKey
            putStr "12 search results for \"Mesh\" query: "
            case response of
              Left e  -> print e
              Right r -> print (artists r)
            putStrLn ""
  where artists = mapM (getContent <=< lookupChild "name") <=< lookupChildren "artist" <=< lookupChild "artistmatches" <=< lookupChild "results" <=< wrap

share :: APIKey -> SessionKey -> IO ()
share apiKey sessionKey = do response <- Artist.share (Artist "Sleep") [Recipient "liblastfm"] (Just $ Message "Just listen!") Nothing apiKey sessionKey
                             case response of
                               Left e  -> print e
                               Right () -> return ()

start :: IO ()
start = do getCorrection
           getEvents
           getImages
           getInfo
           getPastEvents
           getPodcast
           getShouts
           getSimilar
           getTags
           getTopAlbums
           getTopFans
           getTopTags
           getTopTracks
           search
           (apiKey, sessionKey, secret) <- getConfig ".lastfm.conf"
           withSecret secret $ do addTags apiKey sessionKey
                                  getTagsAuth apiKey sessionKey
                                  removeTag apiKey sessionKey
                                  share apiKey sessionKey
                                  -- shout (see User.shout example)
