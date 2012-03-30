module ETasteometer (common, auth) where

import Control.Monad ((<=<))
import Prelude hiding (compare)

import Network.Lastfm.Types
import qualified Network.Lastfm.API.Tasteometer as Tasteometer

import Kludges

compare :: IO ()
compare = parse r f "Score"
  where r = Tasteometer.compare (ValueUser $ User "smpcln") (ValueUser $ User "ingolfr") (Just $ Limit 10) (APIKey "b25b959554ed76058ac220b7b2e0a026")
        f = fmap return . content <=< tag "score" <=< tag "result" <=< tag "comparison"

common :: IO ()
common = compare

auth :: APIKey -> SessionKey -> Secret -> IO ()
auth _ _ _ = return ()
