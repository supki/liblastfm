module Kludges where

import Control.Applicative ((<$>))
import Control.Arrow ((|||))
import Control.Monad (liftM, (<=<))
import Data.ByteString.Lazy.Char8 (ByteString)
import Text.XML.Light

import Network.Lastfm

newtype KludgeResponse = KludgeResponse {unwrap :: Element}

wrap :: ByteString -> Maybe KludgeResponse
wrap = Just . KludgeResponse . (!! 1) . onlyElems . parseXML

tag = lookupChild
tags = lookupChildren
content = getContent

-- | Gets first tag's child with given name
lookupChild :: String -> KludgeResponse -> Maybe KludgeResponse
lookupChild tag' = liftM KludgeResponse . findChild (unqual tag') . unwrap

-- | Gets all tag's children with given name
lookupChildren :: String -> KludgeResponse -> Maybe [KludgeResponse]
lookupChildren tag' = Just . map KludgeResponse . findChildren (unqual tag') . unwrap

-- | Gets tag content
getContent :: KludgeResponse -> Maybe String
getContent = Just . strContent . unwrap

-- | Gets tag attribute content
getAttribute :: String -> KludgeResponse -> Maybe String
getAttribute tag' = findAttr (unqual tag') . unwrap

-- | ...
parse :: Lastfm Response -> (KludgeResponse -> Maybe [String]) -> String -> IO ()
parse r f m = (show ||| show . (f <=< wrap)) <$> r >>= \rs -> putStrLn $ m ++ ": " ++ rs ++ "\n"
