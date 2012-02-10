module ETasteometer (common, auth) where

import Control.Monad ((<=<))
import Prelude hiding (compare)

import Network.Lastfm.Response
import Network.Lastfm.Types
import qualified Network.Lastfm.API.Tasteometer as Tasteometer

import Kludges

compare :: IO ()
compare = do response <- Tasteometer.compare (ValueUser $ User "smpcln") (ValueUser $ User "ingolfr") (Just $ Limit 10) (APIKey "b25b959554ed76058ac220b7b2e0a026")
             putStr "Score: "
             case response of
               Left e -> print e
               Right r -> print $ score r
             putStrLn ""
  where score = getContent <=< lookupChild "score" <=< lookupChild "result" <=< lookupChild "comparison" <=< wrap

common :: IO ()
common = compare

auth :: APIKey -> SessionKey -> IO ()
auth _ _ = return ()
