0.5.0
=======

  * Fixed `http-client`'s `Manager` misuse: every call to `lastfm*` takes a
    `Connection` parameter now and multiple calls should share it.

  * Switched to `network-uri` package

0.4.1.0
=======

  * Updated `lens` dependency

  * Switched to `network-uri` package

0.4.0.0
=======

  * `http-conduit` exceptions aren't propagate further into user code anymore

  * XML responses are parsed into `Document` type from `xml-conduit`

0.3.2.0
=======

  * Fixed `auth.getMobileSession` API method

  * Provided `lastfm_` function for making requests without parsing response. That is most useful for `POST` requests like `N.L.Track.love`

0.3.0.0
=======

  * Supported batch operations for `N.L.Track.scrobble`, `N.L.Library.{addAlbum,addArtist}`

0.2.0.0
=======

  * Vastly simplified internal representations leading to simpler library interface
