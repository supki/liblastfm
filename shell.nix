{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }: let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages(ps: [
    ps.hdevtools ps.doctest ps.hspec-discover ps.hlint
  ]);
  cabal-install = pkgs.haskell.packages.${compiler}.cabal-install;
  pkg = (import ./default.nix { inherit nixpkgs compiler; });
in
  pkgs.stdenv.mkDerivation rec {
    name = pkg.pname;
    buildInputs = [ ghc cabal-install ] ++ pkg.env.buildInputs;
    shellHook = ''
      ${pkg.env.shellHook}
      cabal configure --enable-tests --package-db=$NIX_GHC_LIBDIR/package.conf.d
    '';
  }
