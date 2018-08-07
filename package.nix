{ mkDerivation, aeson, base, bytestring, cereal, containers
, cryptonite, hspec, hspec-expectations-lens, http-client
, http-client-tls, lens, lens-aeson, network-uri, profunctors
, semigroups, stdenv, text, transformers, xml-conduit
, xml-html-conduit-lens
}:
mkDerivation {
  pname = "liblastfm";
  version = "0.6.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cereal containers cryptonite http-client
    http-client-tls network-uri profunctors semigroups text
    transformers xml-conduit
  ];
  testHaskellDepends = [
    aeson base bytestring cereal containers cryptonite hspec
    hspec-expectations-lens http-client http-client-tls lens lens-aeson
    network-uri profunctors text transformers xml-conduit
    xml-html-conduit-lens
  ];
  description = "Lastfm API interface";
  license = stdenv.lib.licenses.mit;
}
