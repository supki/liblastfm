{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

let liblastfm = import ../. {}; in

haskellPackages.cabal.mkDerivation (self: {
  pname = "liblastfm-examples";
  version = "0.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [
    aeson async happstackServer lens lensAeson liblastfm text
    transformers
  ];
  meta = {
    description = "Examples for liblastfm";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
