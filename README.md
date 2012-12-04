##FAQ
**Q: I'm getting the following error. How do I fix it?**
```
    No instance for (Data.Default.Default (R 'Ready f0))
      arising from a use of `lastfm'
    The type variable `f0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there are several potential instances:
      instance Data.Default.Default (R a 'XML)
        -- Defined at src/Network/Lastfm/Request.hs:78:10
      instance Data.Default.Default (R a 'JSON)
        -- Defined at src/Network/Lastfm/Request.hs:69:10
```
A: This error message indicates that you've forgotten to specify response type. 
To fix it, add `<> json` or `<> xml` to offending `Request`. For example:

```haskell
wrong = lastfm (Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890")
right = lastfm (Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890" <> json)
```
-