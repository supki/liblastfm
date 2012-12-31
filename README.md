# liblastfm [![Build Status](https://secure.travis-ci.org/supki/haskell-liblastfm.png?branch=develop)](http://travis-ci.org/supki/haskell-liblastfm)
Complete API interface to [last.fm][1] service.  
Documentation is available in two flavours:
  * original [API reference][2]
  * liblastfm [haddocks][3]

## General introduction
liblastfm provides Applicative interface for constructing requests. Also, it handles all machinery needed to prepare request for sending:
  * [signing][4]
  * url-encoding
  * miscellaneous stuff like choosing correct HTTP method, etc

Once request is ready, liblastfm may send it and get you back a response.
Response format might be:
  * `Maybe Value` from [aeson][5] for json queries (nice interaction with [aeson-lens][6] for free!)
  * raw `ByteString` for xml queries

## Installation
To install either use hackage:

    % cabal install liblastfm
    
Or git:

    % git clone git@github.com:supki/haskell-liblastfm
    % cd haskell-liblastfm
    % cabal install
    
## Usage
Suppose, you need to use [`tag.search`](http://www.last.fm/api/show/tag.search) API method.
First find it in liblastfm: `Tag` would be the name of the module and `search` would be the name of function. [Here it is][7].
So import a couple of modules:

    ghci> import Network.Lastfm -- bunch of useful utilities
    ghci> import qualified Network.Lastfm.Tag as Tag -- for Tag.search

Now you may you applicative `<*>` for required and `<*` or `*>` for optional parameters to construct
desired request:

    Tag.search <*> tag "russian-folk" <* limit 3 <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json

To send constructed request use `lastfm`:

    ghci> lastfm $ Tag.search <*> tag "russian-folk" <* limit 10 <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json
    Just (Object fromList [("results",Object fromList [("tagmatches", ...
    
How to parse responses is described [in wiki][8].

## FAQ
**Q: I'm getting the following error. How do I fix it?**
```
> Artist.getInfo <*> artist "Pink Floyd" <*> apiKey "1234567890"

<interactive>:8:27:
    Couldn't match expected type `Data.Text.Lazy.Internal.Text'
                with actual type `[Char]
```
A: This means you haven't OverloadedStrings extension enabled.
To enable it (either one works):
  * type in `:set -XOverloadedStrings` while in ghci session.
  * add `{-# LANGUAGE OverloadedStrings #-}` to the top of the file
  * compile with `-XOverloadedStrings` switch

**Q: I'm getting the following error. How do I fix it?**
```
> lastfm (Artist.getInfo <*> artist "Pink Floyd" <*> apiKey "1234567890")

<interactive>:13:1:
    No instance for (Network.Lastfm.Response.Supported f0)
      arising from a use of `lastfm'
    The type variable `f0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there are several potential instances:
      instance Network.Lastfm.Response.Supported 'XML
        -- Defined at src/Network/Lastfm/Response.hs:66:10
      instance Network.Lastfm.Response.Supported 'JSON
        -- Defined at src/Network/Lastfm/Response.hs:51:10
```
A: This error message indicates that GHC cannot infer response format for `Request`. 
To fix it, add use `json` or `xml` helpers, depending on your needs

-

 [1]: http://www.last.fm/
 [2]: http://www.last.fm/api/intro
 [3]: http://supki.github.com/haskell-liblastfm/
 [4]: http://www.last.fm/api/authspec#8
 [5]: http://hackage.haskell.org/package/aeson
 [6]: http://hackage.haskell.org/package/aeson-lens
 [7]: http://supki.github.com/haskell-liblastfm/Network-Lastfm-Tag.html#v:search
 [8]: https://github.com/supki/haskell-liblastfm/wiki/How-to-parse-JSON-response
