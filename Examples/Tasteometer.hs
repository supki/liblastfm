#!/usr/bin/env runhaskell

import Control.Monad ((<=<))
import Prelude hiding (compare)

import qualified Network.Lastfm.API.Tasteometer as Tasteometer
import Network.Lastfm.Core
import Network.Lastfm.Types

compare :: IO ()
compare = do response <- Tasteometer.compare (ValueUser $ User "smpcln") (ValueUser $ User "ingolfr") (Just $ Limit 10) (APIKey "b25b959554ed76058ac220b7b2e0a026")
             putStr "Score: "
             case response of
               Left e -> print e
               Right r -> print $ score r
  where score = getContent <=< lookupChild "score" <=< lookupChild "result" <=< lookupChild "comparison"

main = compare
