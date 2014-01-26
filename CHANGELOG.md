0.4.0.0
=======

  * Fancier error handling, i.e. all errors are presented as ADTs instead of exceptions
  * `HttpException`s are catched by `lastfm`
  * XML responses are parsed to xml-conduit's `Document`

0.3.2.0
=======

  * Fix `auth.getMobileSession` method
  * Provide `lastfm_` function for making requests without parsing response. That is most useful for `POST` requests like `N.L.Track.love`

0.3.0.0
=======

  * Support batch operations for `N.L.Track.scrobble`, `N.L.Library.{addAlbum,addArtist}`

0.2.0.0
=======

  * Vastly simplified internal representations leading to simpler library interface
