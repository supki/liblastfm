{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

let
  liblastfm = (import <nixpkgs> {})
    .stdenv.lib.overrideDerivation (import ../. {}) (_: { doCheck = false; } );
in

haskellPackages.cabal.mkDerivation (self: {
  pname = "liblastfm-examples";
  version = "0.5.0";
  src = builtins.filterSource (path: type: type != "unknown") ./.;
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
