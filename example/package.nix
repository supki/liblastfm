{ mkDerivation, aeson, async, base, happstack-server, lens
, lens-aeson, liblastfm, stdenv, text, transformers
}:
mkDerivation {
  pname = "liblastfm-examples";
  version = "0.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base happstack-server lens lens-aeson liblastfm text
    transformers
  ];
  description = "Liblastfm examples";
  license = stdenv.lib.licenses.mit;
}
