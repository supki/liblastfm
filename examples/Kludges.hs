module Kludges where

import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Text.XML.Light

import Network.Lastfm.Types

newtype KludgeResponse = KludgeResponse {unwrap :: Element}

wrap :: String -> Maybe KludgeResponse
wrap = Just . KludgeResponse . (!! 1) . onlyElems . parseXML

-- | Gets first tag's child with given name
lookupChild :: String -> KludgeResponse -> Maybe KludgeResponse
lookupChild tag = liftM KludgeResponse . findChild (unqual tag) . unwrap

-- | Gets all tag's children with given name
lookupChildren :: String -> KludgeResponse -> Maybe [KludgeResponse]
lookupChildren tag = Just . map KludgeResponse . findChildren (unqual tag) . unwrap

-- | Gets tag content
getContent :: KludgeResponse -> Maybe String
getContent = Just . strContent . unwrap

-- | Gets tag attribute content
getAttribute :: String -> KludgeResponse -> Maybe String
getAttribute tag = findAttr (unqual tag) . unwrap
