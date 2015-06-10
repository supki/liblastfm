{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module SpecHelper
  ( -- * Expectations
    Fold
  , shouldHaveResponse
  , shouldHaveJson
  , shouldHaveJson_
  , shouldHaveXml
  , shouldHaveXml_
    -- * public data
  , publicly
  , publicKey
    -- * private data
    -- $awful
  , privately
  , privateAPIKey
  , privateSessionKey
  , privateSecret
    -- $really-awful
  , man
  ) where

import           Control.Exception (Exception, throwIO)
import           Control.Lens
import           Data.Aeson (Value)
import           Data.Aeson.Lens
import           Data.Text (Text, pack)
import           Data.Typeable (Typeable)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import           Network.Lastfm
import           System.Environment
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec
import           Test.HUnit (assertFailure)
import           Text.Printf
import           Text.Xml.Lens
import           Unsafe.Coerce (unsafeCoerce)

infixl 1 `shouldHaveJson`, `shouldHaveXml`


-- | Inspect 'Response' with 'Query'
shouldHaveResponse :: (Show r, Supported f r) => Request f Ready -> Fold r a -> Expectation
shouldHaveResponse q l = do
  r <- lastfm man q
  case preview (_Right.l) r of
    Just _  -> return ()
    Nothing -> assertFailure (printf "Query failed on %s" (show r))

shouldHaveJson :: Request 'JSON Ready -> Fold Value a -> Expectation
shouldHaveJson = shouldHaveResponse

shouldHaveXml :: Request 'XML Ready -> Fold Document a -> Expectation
shouldHaveXml = shouldHaveResponse

-- | Check success stuff for POST requests
shouldHaveJson_ :: Request 'JSON Ready -> Expectation
shouldHaveJson_ l = shouldHaveResponse l (key "status".only "ok")

shouldHaveXml_ :: Request 'XML Ready -> Expectation
shouldHaveXml_ l = shouldHaveResponse l (root.attributed (ix "status".only "ok"))

-- | Make a request using public API key
publicly :: Request f (APIKey -> Ready) -> Request f Ready
publicly r = r <*> publicKey

-- | API Key used for requests that do not require authentification
publicKey :: Request f APIKey
publicKey = apiKey "234fc6e0f41f6ef99b7bd62ebaf8d318"

-- | Environment is missing a variable
data EnvironmentMissing = EnvironmentMissing String deriving (Show, Eq, Typeable)

instance Exception EnvironmentMissing

-- $awful

-- | Make a request signed by a secret using private API and session keys
privately :: Request f (APIKey -> SessionKey -> Sign) -> Request f Ready
privately r = sign privateSecret $ r <*> privateAPIKey <*> privateSessionKey

privateAPIKey :: Request f APIKey
privateAPIKey = apiKey (liblastfmEnv "HASKELL_LIBLASTFM_APIKEY")

privateSessionKey :: Request f SessionKey
privateSessionKey = sessionKey (liblastfmEnv "HASKELL_LIBLASTFM_SESSIONKEY")

privateSecret :: Secret
privateSecret = Secret (liblastfmEnv "HASKELL_LIBLASTFM_SECRET")

-- | Get an environment variable or throw a 'EnvironmentMissing' exception
liblastfmEnv :: String -> Text
liblastfmEnv var = unsafePerformIO $ do
  mv <- lookupEnv var
  case mv of
    Just v  -> return (pack v)
    Nothing -> throwIO (EnvironmentMissing var)

-- $really-awful

man :: Connection
man = unsafeCoerce (unsafePerformIO (Http.newManager Http.tlsManagerSettings))
