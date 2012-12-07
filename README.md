##FAQ
**Q: I'm getting the following error. How do I fix it?**
```
> Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890"

<interactive>:8:27:
    Couldn't match type `[Char]' with `Data.Text.Lazy.Internal.Text'
    Expected type: Artist
      Actual type: [Char]
```
A: This means you haven't `-XOverloadedStrings` extension enabled in current ghci session.
To enable it type in `:set -XOverloadedStrings`.

**Q: I'm getting the following error. How do I fix it?**
```
> lastfm (Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890")

<interactive>:3:1:
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
wrong <- lastfm (Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890")
right <- lastfm (Album.getInfo <> artist "Pink Floyd" <> album "The Wall" <> apiKey "1234567890" <> json)
```
This problem should not arise while writing applications since hopefully you will have enough
type annotations for inference to work out. But `json` and `xml` are still useful in ghci.

-