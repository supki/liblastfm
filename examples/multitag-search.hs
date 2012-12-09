#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{- This example shows how to multitag search could be kludged
 -
 - Sample output:
 -
 - $> ./examples/multitag-search.hs "j-pop" "anime" "anime-ost"
 - 坂本真綾
 - KOTOKO
 - Lisa
 - 茅原実里
 -}
import           Control.Applicative ((<$>))
import           Control.Lens
import           Data.Aeson.Lens
import           Data.List           (intersect)
import           Data.Text           (Text)
import qualified Data.Text.IO        as T
import           Data.Text.Lazy      (pack)
import           System.Environment  (getArgs)

import           Network.Lastfm
import           Network.Lastfm.Tag


main ∷ IO ()
main = cout =<< get_artists =<< read_args
  where
    cout = mapM_ T.putStrLn
    read_args = Prelude.map Data.Text.Lazy.pack <$> getArgs


get_artists ∷ [Tag] → IO [Text]
get_artists = (foldl1 intersect <$>) . mapM ((get_name <$>) . query)
  where
    query τ = lastfm $ getTopArtists τ <> limit 100 <> apiKey "29effec263316a1f8a97f753caaa83e0" <> json
    get_name r = (r ^. key "topartists" . key "artist") ^.. folded . traverseArray .  key "name" . asText ^.. folded . folded
