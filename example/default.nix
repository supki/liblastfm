{ cabal, aeson, async, cereal, contravariant, cryptoApi, happstackServer, hspec
, hspecExpectationsLens, httpClient, httpClientTls, HUnit, lens, lensAeson, networkUri
, profunctors, pureMD5, semigroups, text, transformers, void, xmlConduit
, xmlHtmlConduitLens
}:

let liblastfm = import ../. {
  inherit cabal aeson cereal contravariant cryptoApi hspec hspecExpectationsLens
  httpClient httpClientTls HUnit lens lensAeson networkUri profunctors pureMD5
  semigroups text void xmlConduit xmlHtmlConduitLens;
}; in

cabal.mkDerivation (self: {
  pname = "liblastfm-examples";
  version = "0.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson async happstackServer lens lensAeson liblastfm text
    transformers
  ];
  meta = {
    description = "Examples for liblastfm";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
