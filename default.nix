{ cabal, aeson, cereal, contravariant, cryptoApi, hspec
, hspecExpectationsLens, httpClient, httpClientTls, HUnit, lens
, lensAeson, networkUri, profunctors, pureMD5, semigroups, text
, void, xmlConduit, xmlHtmlConduitLens
}:

cabal.mkDerivation (self: rec {
  pname = "liblastfm";
  version = "0.5.0";
  src = ./.;
  buildDepends = [
    aeson cereal contravariant cryptoApi httpClient httpClientTls
    networkUri profunctors pureMD5 semigroups text void xmlConduit
  ];
  testDepends = buildDepends ++ [ hspec hspecExpectationsLens HUnit lens lensAeson xmlHtmlConduitLens ];
  meta = {
    description = "Lastfm API interface";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
