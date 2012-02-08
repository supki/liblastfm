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
start = do (apiKey, sessionKey, secret) <- getConfig ".lastfm.conf"
           withSecret secret $ do addTags apiKey sessionKey
                                  ban apiKey sessionKey
                                  love apiKey sessionKey
                                  removeTag apiKey sessionKey
                                  share apiKey sessionKey
                                  unban apiKey sessionKey
                                  unlove apiKey sessionKey
