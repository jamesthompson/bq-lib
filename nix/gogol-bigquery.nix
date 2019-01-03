{ mkDerivation, base, fetchgit, gogol-core, stdenv }:
mkDerivation {
  pname = "gogol-bigquery";
  version = "0.3.0";
  src = fetchgit {
    url = "git://github.com/folq/gogol";
    sha256 = "1567cl7s2mxc81qkmwlax8lbz97rw368qbz2aw5dgpd65f7n4sgy";
    rev = "c43cc3ed68b5b7ab286b592a15d9345e38ea5d0d";
  };
  postUnpack = "sourceRoot+=/gogol-bigquery; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base gogol-core ];
  homepage = "https://github.com/brendanhay/gogol";
  description = "Google BigQuery SDK";
  license = "unknown";
}
