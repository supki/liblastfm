#liblastfm [![Build Status](https://secure.travis-ci.org/supki/haskell-liblastfm.png?branch=develop)](http://travis-ci.org/supki/haskell-liblastfm)
Complete API interface to [last.fm][1] service.  
Documentation is available in two flavours:
  * original [API reference][2]
  * liblastfm [haddocks][3]

##FAQ
**Q: I'm getting the following error. How do I fix it?**
```
> Album.getInfo <*> artist "Pink Floyd" <*> album "The Wall" <*> apiKey "1234567890"

<interactive>:8:27:
    Couldn't match type `[Char]' with `Data.Text.Lazy.Internal.Text'
    Expected type: Artist
      Actual type: [Char]
```
A: This means you haven't `-XOverloadedStrings` extension enabled.
To enable it:
  * type in `:set -XOverloadedStrings` while in ghci session.
  * add `{-# LANGUAGE OverloadedStrings #-}` to the top of the file
  * compile with `-XOverloadedStrings` switch

**Q: I'm getting the following error. How do I fix it?**
```
> lastfm (Album.getInfo <*> artist "Pink Floyd" <*> album "The Wall" <*> apiKey "1234567890")

<interactive>:3:1:
    No instance for (Data.Default.Default (R f0 'Send))
      arising from a use of `lastfm'
    The type variable `f0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there are several potential instances:
      instance Data.Default.Default (R 'XML a)
        -- Defined at src/Network/Lastfm/Request.hs:78:10
      instance Data.Default.Default (R 'JSON a)
        -- Defined at src/Network/Lastfm/Request.hs:69:10
```
A: This error message indicates that GHC cannot infer response format for `Request`. 
To fix it, add use `json` or `xml` helpers, depending on your needs

-

 [1]: http://www.lastfm.ru/
 [2]: http://www.lastfm.ru/api/intro
 [3]: http://supki.github.com/haskell-liblastfm/
