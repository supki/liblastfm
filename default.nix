{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: rec {
  pname = "liblastfm";
  version = "0.5.0";
  src = ./.;
  buildDepends = with haskellPackages; [
    aeson cereal contravariant cryptoApi httpClient httpClientTls
    networkUri profunctors pureMD5 semigroups text void xmlConduit
  ];
  testDepends = with haskellPackages; buildDepends ++ [
    hspec hspecExpectationsLens HUnit lens lensAeson xmlHtmlConduitLens
  ];
  meta = {
    description = "Lastfm API interface";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
  doCheck = false;
})
