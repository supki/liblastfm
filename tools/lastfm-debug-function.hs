{-# LANGUAGE UnicodeSyntax #-}
module Network.Lastfm.Debug
  ( debugSimple
  , debugAuth
  ) where

import Control.Applicative ((<$>))
import Data.Char (isSpace)

import Network.Lastfm


-- Parse configuration file in given format:
-- APIKey = …
-- SessionKey = …
-- Secret = …
getConfig ∷ FilePath → IO (APIKey, SessionKey, Secret)
getConfig fp = do
  (apiKey:sessionKey:secret:_) ← map (drop 1 . dropWhile (/= '=') . filter (not . isSpace)) . lines <$> readFile fp
  return (APIKey apiKey, SessionKey sessionKey, Secret secret)


-- | Debug a function that doesn't need authentication
debugSimple ∷ FilePath -- ^ Configuration file path
            → (APIKey → Lastfm Response) -- ^ Function to debug
            → Lastfm Response -- ^ Liblastfm response
debugSimple fp f = debugAuth fp $ \ak _ _ → f ak


-- | Debug a fancy function
debugAuth ∷ FilePath -- ^ Configuration file path
          → (APIKey → SessionKey → Secret → Lastfm Response) -- ^ Function to debug
          → Lastfm Response -- ^ Liblastfm response
debugAuth fp f = do
  (ak, sk, s) ← getConfig fp
  f ak sk s
