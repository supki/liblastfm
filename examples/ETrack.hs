module ETrack (start) where

import Control.Monad ((<=<))

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Track as Track

import Kludges

apiKey = APIKey "b25b959554ed76058ac220b7b2e0a026"

addTags :: APIKey -> SessionKey -> IO ()
addTags apiKey sessionKey = do response <- Track.addTags (Artist "Jefferson Airplane") (Track "White rabbit") [Tag "60s", Tag "awesome"] apiKey sessionKey
                               case response of
                                 Left e   -> print e
                                 Right () -> return ()

ban :: APIKey -> SessionKey -> IO ()
ban apiKey sessionKey = do response <- Track.ban (Artist "Eminem") (Track "Kim") apiKey sessionKey
                           case response of
                             Left e   -> print e
                             Right () -> return ()

getBuylinks :: IO ()
getBuylinks = do response <- Track.getBuyLinks (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing (Country "United Kingdom") apiKey
                 putStr "Download suppliers: "
                 case response of
                   Left e  -> print e
                   Right r -> print (suppliers r)
                 putStrLn ""
  where suppliers = mapM (getContent <=< lookupChild "supplierName") <=< lookupChildren "affiliation" <=< lookupChild "downloads" <=< lookupChild "affiliations" <=< wrap

getCorrection :: IO ()
getCorrection = do response <- Track.getCorrection (Artist "Pink Ployd") (Track "Brain Damage") apiKey
                   putStr "Correction: "
                   case response of
                     Left e  -> print e
                     Right r -> print (correction r)
                   putStrLn ""
  where correction = getContent <=< lookupChild "name" <=< lookupChild "artist" <=< lookupChild "track" <=< lookupChild "correction" <=< lookupChild "corrections" <=< wrap

getTags :: IO ()
getTags = do response <- Track.getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Left $ User "liblastfm") apiKey
             putStr "White Rabbit tags: "
             case response of
               Left e  -> print e
               Right r -> print (tags r)
             putStrLn ""
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "tags" <=< wrap

getTagsAuth :: APIKey -> SessionKey -> IO ()
getTagsAuth apiKey sessionKey = do response <- Track.getTags (Left (Artist "Jefferson Airplane", Track "White Rabbit")) Nothing (Right sessionKey) apiKey
                                   putStr "White rabbit tags: "
                                   case response of
                                     Left e  -> print e
                                     Right r -> print (tags r)
                                   putStrLn ""
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "tags" <=< wrap

getTopFans :: IO ()
getTopFans = do response <- Track.getTopFans (Left (Artist "Pink Floyd", Track "Comfortably Numb")) Nothing apiKey
                print response
                case response of
                  Left e  -> print e
                  Right r -> print (fans r)
                putStrLn ""
  where fans = mapM (getContent <=< lookupChild "name") <=< lookupChildren "user" <=< lookupChild "topfans" <=< wrap

getTopTags :: IO ()
getTopTags = do response <- Track.getTopTags (Left (Artist "Pink Floyd", Track "Brain Damage")) Nothing apiKey
                putStr "Top tags: "
                case response of
                  Left e  -> print e
                  Right r -> print (tags r)
                putStrLn ""
  where tags = mapM (getContent <=< lookupChild "name") <=< lookupChildren "tag" <=< lookupChild "toptags" <=< wrap

love :: APIKey -> SessionKey -> IO ()
love apiKey sessionKey = do response <- Track.love (Artist "Gojira") (Track "Ocean") apiKey sessionKey
                            case response of
                              Left e   -> print e
                              Right () -> return ()

removeTag :: APIKey -> SessionKey -> IO ()
removeTag apiKey sessionKey = do response <- Track.removeTag (Artist "Jefferson Airplane") (Track "White rabbit") (Tag "awesome") apiKey sessionKey
                                 case response of
                                   Left e   -> print e
                                   Right () -> return ()

share :: APIKey -> SessionKey -> IO ()
share apiKey sessionKey = do response <- Track.share (Artist "Led Zeppelin") (Track "When the Levee Breaks") [Recipient "liblastfm"] (Just $ Message "Just listen!") Nothing apiKey sessionKey
                             case response of
                               Left e  -> print e
                               Right () -> return ()

unban :: APIKey -> SessionKey -> IO ()
unban apiKey sessionKey = do response <- Track.unban (Artist "Eminem") (Track "Kim") apiKey sessionKey
                             case response of
                               Left e   -> print e
                               Right () -> return ()

unlove :: APIKey -> SessionKey -> IO ()
unlove apiKey sessionKey = do response <- Track.unlove (Artist "Gojira") (Track "Ocean") apiKey sessionKey
                              case response of
                                Left e   -> print e
                                Right () -> return ()

start :: IO ()
start = do getBuylinks
           getCorrection
           getTags
           getTopFans
           getTopTags
           (apiKey, sessionKey, secret) <- getConfig ".lastfm.conf"
           withSecret secret $ do addTags apiKey sessionKey
                                  getTagsAuth apiKey sessionKey
                                  ban apiKey sessionKey
                                  love apiKey sessionKey
                                  removeTag apiKey sessionKey
                                  share apiKey sessionKey
                                  unban apiKey sessionKey
                                  unlove apiKey sessionKey
