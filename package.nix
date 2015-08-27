{ mkDerivation, aeson, base, bytestring, cereal, containers
, crypto-api, hspec, hspec-expectations-lens, http-client
, http-client-tls, HUnit, lens, lens-aeson, network-uri
, profunctors, pureMD5, semigroups, stdenv, text, xml-conduit
, xml-html-conduit-lens
}:
mkDerivation {
  pname = "liblastfm";
  version = "0.5.1";
  src = ./.;
  buildDepends = [
    aeson base bytestring cereal containers crypto-api http-client
    http-client-tls network-uri profunctors pureMD5 semigroups text
    xml-conduit
  ];
  testDepends = [
    aeson base bytestring cereal containers crypto-api hspec
    hspec-expectations-lens http-client http-client-tls HUnit lens
    lens-aeson network-uri profunctors pureMD5 text xml-conduit
    xml-html-conduit-lens
  ];
  description = "Lastfm API interface";
  license = stdenv.lib.licenses.mit;
}
