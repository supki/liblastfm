{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Helper
  ( Query
  , query, ok
  , publicKey
  ) where

import Control.Lens
import Control.Lens.Aeson
import Network.Lastfm (Request, Response, Format(..), Ready, APIKey, lastfm, apiKey)
import Test.HUnit
import Text.Printf


type Query a = Fold (Response JSON) a


-- | Inspect 'Response' with 'Query'
query :: Query a -> Request JSON Ready -> Assertion
query l q = do
  r <- lastfm q
  case r of
    Left e  -> assertFailure (printf "last.fm error: %s" (show e))
    Right val ->
      case preview l val of
        Just _  -> assertBool "OK" True
        Nothing -> assertFailure (printf "bad JSON object: %s" (show val))

-- | Check success stuff for POST requests
ok :: Query ()
ok = key "status".only "ok"


-- | Key used for requests that do not require authentification
publicKey :: Request f APIKey
publicKey = apiKey "234fc6e0f41f6ef99b7bd62ebaf8d318"
