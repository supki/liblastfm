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
A: This error message indicates that GHC cannot infer response format for that Request. 
To fix it, add use `json` or `xml` helpers, depending on your needs:

```haskell
wrong = lastfm (Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890")
right = lastfm (Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890" <> json)
```
Another way to solve the problem would be to add explicit type signature `right :: Request Ready JSON`.
You probably will do that in applications, but `json` and `xml` are still useful in ghci.

-