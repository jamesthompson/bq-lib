{ mkDerivation, aeson, attoparsec, base, bifunctors, bytestring
, case-insensitive, conduit, dlist, exceptions, fetchgit, hashable
, http-api-data, http-client, http-media, http-types, lens
, resourcet, scientific, servant, stdenv, tasty, text, time
, unordered-containers
}:
mkDerivation {
  pname = "gogol-core";
  version = "0.3.0";
  src = fetchgit {
    url = "git://github.com/folq/gogol";
    sha256 = "1567cl7s2mxc81qkmwlax8lbz97rw368qbz2aw5dgpd65f7n4sgy";
    rev = "c43cc3ed68b5b7ab286b592a15d9345e38ea5d0d";
  };
  postUnpack = "sourceRoot+=/core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base bifunctors bytestring case-insensitive
    conduit dlist exceptions hashable http-api-data http-client
    http-media http-types lens resourcet scientific servant text time
    unordered-containers
  ];
  testHaskellDepends = [ base tasty ];
  homepage = "https://github.com/brendanhay/gogol";
  description = "Core data types and functionality for Gogol libraries";
  license = "unknown";
}
