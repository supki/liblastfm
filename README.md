liblastfm
=========
[![Hackage](https://budueba.com/hackage/liblastfm)](https://hackage.haskell.org/package/liblastfm)
[![Build Status](https://drone.io/github.com/supki/liblastfm/status.png)](https://drone.io/github.com/supki/liblastfm/latest)
[![Build Status](https://secure.travis-ci.org/supki/liblastfm.png?branch=develop)](https://travis-ci.org/supki/liblastfm)

Complete API interface to [last.fm][last.fm] service.
Documentation is available in two flavours:

  * original [API reference][last.fm/api]

  * liblastfm [haddocks][liblastfm/haddocks]

Introduction
------------
liblastfm provides Applicative interface for constructing requests. Also, it handles all machinery needed to prepare request for sending:

  * [signing][last.fm/sign]

  * url-encoding

  * miscellaneous stuff like choosing correct HTTP method, etc

Once request is ready, liblastfm can send it and get you back a response.
Response format might be:

  * [aeson][aeson] `Value` for json queries

  * [xml-conduit][xml-conduit] `Document` for xml queries

Installation
------------
To install either use hackage:

    % cabal install liblastfm

Or git:

    % git clone git@github.com:supki/liblastfm
    % cd liblastfm
    % cabal install

Usage
-----
Suppose, you need to use [`tag.search`][last.fm/api-usage] API method.
First find it in liblastfm: `Tag` would be the name of the module and `search` would be the name of function. [Here it is][liblastfm/haddocks-usage].
So import a couple of modules:

    >>> import           Network.Lastfm            -- a bunch of useful utilities
    >>> import qualified Network.Lastfm.Tag as Tag -- for Tag.search

Now you may you applicative `<*>` for required and `<*` or `*>` for optional parameters to construct
desired request:

    Tag.search <*> tag "russian-folk" <* limit 3 <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json

To send constructed request use `lastfm`:

    >>> lastfm $ Tag.search <*> tag "russian-folk" <* limit 10 <*> apiKey "29effec263316a1f8a97f753caaa83e0" <* json
    Just (Object fromList [("results",Object fromList [("tagmatches", ...

[Wiki][liblastfm/wiki] describes how to parse responses.

FAQ
---

**Q: I'm getting the following error. How do I fix it?**

```
>>> Artist.getInfo <*> artist "Pink Floyd" <*> apiKey "29effec263316a1f8a97f753caaa83e0"

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
>>> lastfm (Artist.getInfo <*> artist "Pink Floyd" <*> apiKey "29effec263316a1f8a97f753caaa83e0")

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

A: This error message indicates that GHC cannot infer response format for the `Request`.
To fix it, add use `json` or `xml` helpers, depending on your needs

A note on testing
-----------------

To test Lastfm API compatibility (`api` test suite)—specifically, authentication requiring
examples—you will need to set `HASKELL_LIBLASTFM_APIKEY`, `HASKELL_LIBLASTFM_SESSIONKEY`,
and `HASKELL_LIBLASTFM_SECRET` environment variables to your api key, session key, and
secret respectively; for example (bash):

```bash
export HASKELL_LIBLASTFM_APIKEY="__API_KEY__"
export HASKELL_LIBLASTFM_SESSIONKEY="__SESSION_KEY__"
export HASKELL_LIBLASTFM_SECRET="__SECRET__"
```

Please, consult Lastfm API documentation and  `examples/*-authentication.hs`
examples if you don't know where to get your credentials.

 [last.fm]: http://www.last.fm/
 [last.fm/api]: http://www.last.fm/api/intro
 [last.fm/api-usage]: http://www.last.fm/api/show/tag.search
 [last.fm/sign]: http://www.last.fm/api/authspec#8
 [liblastfm/haddocks]: http://supki.github.io/liblastfm/
 [liblastfm/haddocks-usage]: http://supki.github.com/liblastfm/Network-Lastfm-Tag.html#v:search
 [liblastfm/wiki]: https://github.com/supki/liblastfm/wiki/How-to-parse-JSON-response
 [aeson]: https://hackage.haskell.org/package/aeson
 [xml-conduit]: https://hackage.haskell.org/package/xml-conduit
