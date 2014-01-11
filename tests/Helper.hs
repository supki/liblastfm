{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Helper
  ( Query
  , query, ok
  ) where

import Control.Lens
import Control.Lens.Aeson
import Network.Lastfm (Request, Response, Format(..), Ready, lastfm)
import Test.HUnit
import Text.Printf


type Query a = Fold (Response JSON) a


query :: Query a -> Request JSON Ready -> Assertion
query l q = do
  r <- lastfm q
  case r of
    Left e  -> assertFailure (printf "last.fm error: %s" (show e))
    Right val ->
      case preview l val of
        Just _  -> assertBool "OK" True
        Nothing -> assertFailure (printf "bad JSON object: %s" (show val))

ok :: Query ()
ok = key "status".only "ok"
