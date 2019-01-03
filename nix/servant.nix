{ mkDerivation, aeson, aeson-compat, attoparsec, base, base-compat
, bytestring, Cabal, cabal-doctest, case-insensitive, directory
, doctest, filemanip, filepath, hspec, hspec-discover
, http-api-data, http-media, http-types, mmorph, mtl
, natural-transformation, network-uri, QuickCheck
, quickcheck-instances, stdenv, string-conversions, tagged, text
, url, vault
}:
mkDerivation {
  pname = "servant";
  version = "0.12.1";
  sha256 = "3003682bc4cf9fea5616561d985054c3b3c9965d1fcd985eb59bfef0a9db76aa";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring case-insensitive
    http-api-data http-media http-types mmorph mtl
    natural-transformation network-uri string-conversions tagged text
    vault
  ];
  testHaskellDepends = [
    aeson aeson-compat attoparsec base base-compat bytestring directory
    doctest filemanip filepath hspec QuickCheck quickcheck-instances
    string-conversions text url
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
