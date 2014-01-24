{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE RankNTypes #-}
module SpecHelper
  ( Query
  , query
  , query_
  , publicKey
  , privateAPIKey
  , privateSessionKey
  , privateSecret
  ) where

import Control.Exception (Exception, throwIO)
import Control.Lens
import Control.Lens.Aeson
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import Network.Lastfm
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.HUnit (assertFailure)
import Text.Printf


type Query a = Fold (Response JSON) a

-- | Inspect 'Response' with 'Query'
query :: Query a -> Request JSON Ready -> Expectation
query l q = do
  r <- lastfm q
  case preview (_Right.l) r of
    Just _  -> return ()
    Nothing -> assertFailure (printf "Query failed on %s" (show r))

-- | Check success stuff for POST requests
query_ :: Request JSON Ready -> Expectation
query_ = query (key "status".only "ok")

-- | API Key used for requests that do not require authentification
publicKey :: Request f APIKey
publicKey = apiKey "234fc6e0f41f6ef99b7bd62ebaf8d318"

-- | Environment is missing a variable
data EnvironmentMissing = EnvironmentMissing String deriving (Show, Eq, Typeable)

instance Exception EnvironmentMissing

-- $awful

-- | Get an environment variable or throw a 'EnvironmentMissing' exception
liblastfmEnv :: String -> Text
liblastfmEnv var = unsafePerformIO $ do
  mv <- lookupEnv var
  case mv of
    Just v  -> return (pack v)
    Nothing -> throwIO (EnvironmentMissing var)

privateAPIKey :: Request f APIKey
privateAPIKey = apiKey (liblastfmEnv "HASKELL_LIBLASTFM_APIKEY")

privateSessionKey :: Request f SessionKey
privateSessionKey = sessionKey (liblastfmEnv "HASKELL_LIBLASTFM_SESSIONKEY")

privateSecret :: Secret
privateSecret = Secret (liblastfmEnv "HASKELL_LIBLASTFM_SECRET")
