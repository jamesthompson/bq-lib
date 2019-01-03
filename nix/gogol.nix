{ mkDerivation, aeson, base, bytestring, case-insensitive, conduit
, conduit-extra, cryptonite, directory, exceptions, fetchgit
, filepath, gogol-core, http-client, http-conduit, http-media
, http-types, lens, memory, mime-types, mtl, resourcet, stdenv
, text, time, transformers, unliftio-core, unordered-containers
, x509, x509-store
}:
mkDerivation {
  pname = "gogol";
  version = "0.3.0";
  src = fetchgit {
    url = "git://github.com/folq/gogol";
    sha256 = "1567cl7s2mxc81qkmwlax8lbz97rw368qbz2aw5dgpd65f7n4sgy";
    rev = "c43cc3ed68b5b7ab286b592a15d9345e38ea5d0d";
  };
  postUnpack = "sourceRoot+=/gogol; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive conduit conduit-extra
    cryptonite directory exceptions filepath gogol-core http-client
    http-conduit http-media http-types lens memory mime-types mtl
    resourcet text time transformers unliftio-core unordered-containers
    x509 x509-store
  ];
  homepage = "https://github.com/brendanhay/gogol";
  description = "Comprehensive Google Services SDK";
  license = "unknown";
}
