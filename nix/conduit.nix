{ mkDerivation, base, containers, criterion, deepseq, exceptions
, hspec, kan-extensions, lifted-base, mmorph, monad-control, mtl
, mwc-random, primitive, QuickCheck, resourcet, safe, split, stdenv
, transformers, transformers-base, transformers-compat, vector
}:
mkDerivation {
  pname = "conduit";
  version = "1.2.13.1";
  sha256 = "77b6b531b52331e3f9d24fb537433d287904354e2cc18eb786fbc2b7aa6cb3a0";
  libraryHaskellDepends = [
    base exceptions lifted-base mmorph monad-control mtl primitive
    resourcet transformers transformers-base transformers-compat
  ];
  testHaskellDepends = [
    base containers exceptions hspec mtl QuickCheck resourcet safe
    split transformers
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq hspec kan-extensions mwc-random
    transformers vector
  ];
  homepage = "http://github.com/snoyberg/conduit";
  description = "Streaming data processing library";
  license = stdenv.lib.licenses.mit;
}
