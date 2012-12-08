# How to parse JSON response

Suppose, you wanna call [`Tag.search`](http://www.last.fm/api/show/tag.search) method.

[Here it is](http://budueba.com/liblastfm/Network-Lastfm-Tag.html#v:search) in liblastfm:

    search :: Tag -> Request Ready f

At first, you need to import `liblastfm` modules:

    ghci> :m + Network.Lastfm Network.Lastfm.Tag

Then - `aeson-lens` and `lens` modules:

    ghci> :m + Data.Aeson.Lens Control.Lens

You may construct your call to something like that using like (remember, `json` part is necessary):

    search "russian-folk" <> limit 3 <> apiKey "29effec263316a1f8a97f753caaa83e0" <> json

What will your api call looks like?

    ghci> lastfm (search "russian-folk" <> limit 10 <> apiKey "29effec263316a1f8a97f753caaa83e0" <> json)
    Just (Object fromList [("results",Object fromList [("tagmatches",Object fromList [("tag",Array (fromList [Object fromList [("name",String "russian folk"),("count",String "3322"),("url",String "www.last.fm/tag/russian%20folk")],Object fromList [("name",String "russian folk metal"),("count",String "335"),("url",String "www.last.fm/tag/russian%20folk%20metal")],Object fromList [("name",String "russian folk rock"),("count",String "150"),("url",String "www.last.fm/tag/russian%20folk%20rock")],Object fromList [("name",String "russian folk-rock"),("count",String "30"),("url",String "www.last.fm/tag/russian%20folk-rock")],Object fromList [("name",String "russian folk music"),("count",String "49"),("url",String "www.last.fm/tag/russian%20folk%20music")],Object fromList [("name",String "russian folk songs"),("count",String "14"),("url",String "www.last.fm/tag/russian%20folk%20songs")],Object fromList [("name",String "russian folk ambient"),("count",String "6"),("url",String "www.last.fm/tag/russian%20folk%20ambient")],Object fromList [("name",String "russian folk pagan metal"),("count",String "6"),("url",String "www.last.fm/tag/russian%20folk%20pagan%20metal")],Object fromList [("name",String "russian folk punk"),("count",String "5"),("url",String "www.last.fm/tag/russian%20folk%20punk")],Object fromList [("name",String "russian folk-russo"),("count",String "7"),("url",String "www.last.fm/tag/russian%20folk-russo")]]))]),("opensearch:startIndex",String "0"),("opensearch:itemsPerPage",String "10"),("opensearch:Query",Object fromList [("#text",String ""),("role",String "request"),("startPage",String "1"),("searchTerms",String "russian-folk")]),("@attr",Object fromList [("for",String "russian-folk")]),("opensearch:totalResults",String "20")])])

Btw, there is a nice package `aeson-pretty` to make such response more readable:

    ghci> import qualified Data.ByteString.Lazy.Char8 as BSC
    ghci> :m + Data.Aeson.Encode.Pretty
    ghci> BSC.putStrLn =<< encodePretty <$> lastfm (search "russian-folk" <> limit 3 <> apiKey "29effec263316a1f8a97f753caaa83e0" <> json)
    {
        "results": {
            "tagmatches": {
                "tag": [
                    {
                        "name": "russian folk",
                        "count": "3322",
                        "url": "www.last.fm/tag/russian%20folk"
                    },
                    {
                        "name": "russian folk metal",
                        "count": "335",
                        "url": "www.last.fm/tag/russian%20folk%20metal"
                    },
                    {
                        "name": "russian folk rock",
                        "count": "150",
                        "url": "www.last.fm/tag/russian%20folk%20rock"
                    }
                ]
            },
            "opensearch:startIndex": "0",
            "opensearch:itemsPerPage": "3",
            "opensearch:Query": {
                "#text": "",
                "role": "request",
                "startPage": "1",
                "searchTerms": "russian-folk"
            },
            "@attr": {
                "for": "russian-folk"
            },
            "opensearch:totalResults": "20"
        }
    }
    it :: ()

So, what we've got here is `Maybe Data.Aeson.Types.Internal.Value`.  
But how could you get all `tag` names from it?  
It's time to use some `aeson-lens` magick  
!

Basically, we want to get `results / tagmatches / tag` part and extract all `name` values.

    ghci> let get_tags = (\r -> (r ^. key "results" . key "tagmatches" . key "tag" ) ^.. folded . traverseArray .  key "name" . asText)
    get_tags ::
      Maybe aeson-0.6.0.2:Data.Aeson.Types.Internal.Value
      -> [Maybe Data.Text.Internal.Text]

And we can get what we want:

    ghci> get_tags <$> lastfm (search "russian-folk" <> limit 3 <> apiKey "29effec263316a1f8a97f753caaa83e0" <> json)
    [Just "russian folk",Just "russian folk metal",Just "russian folk rock"]

[`catMaybes`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Maybe.html#v:catMaybes) to the rescue!

    ghci> catMaybes . get_tags <$> lastfm (search "russian-folk" <> limit 3 <> apiKey "29effec263316a1f8a97f753caaa83e0" <> json)
    ["russian folk","russian folk metal","russian folk rock"]

Done  
!

