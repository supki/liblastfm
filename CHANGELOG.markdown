0.7.0
=====

0.6.1
=====

  * Set default schema to 'https' (issue #27 fixed).

0.6.0
=====

  * Stopped using the deprecated stuff from http-client.

  * Removed `N.L.Response.closeConnection` as it's become a no-op.

  * Removed the "Network" prefix from the modules' names.

  * Set default schema to 'http', since ws.audioscrobbler.com has no proper SSL support (issue #27).

0.5.1
=====

  * Updated documentation to reflect the changes in `0.5.0`.

  * Dropped `void` and `contravariant` dependencies

0.5.0
=====

  * Fixed `http-client`'s `Manager` misuse: every call to `lastfm*` takes a
    `Connection` parameter now and multiple calls should share it.

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
