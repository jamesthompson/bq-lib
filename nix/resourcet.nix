{ mkDerivation, base, containers, exceptions, hspec, lifted-base
, mmorph, monad-control, mtl, stdenv, transformers
, transformers-base, transformers-compat, unliftio-core
}:
mkDerivation {
  pname = "resourcet";
  version = "1.1.11";
  sha256 = "346ed5c3eca87e1b2df5ca97419bd896e27ad39d997b8eea5b62f67c98a824d9";
  revision = "1";
  editedCabalFile = "09sgrzaaishx645hrfflxckyaq0dwk22agjf4sz8nwjafyv3ssh9";
  libraryHaskellDepends = [
    base containers exceptions lifted-base mmorph monad-control mtl
    transformers transformers-base transformers-compat unliftio-core
  ];
  testHaskellDepends = [ base hspec lifted-base transformers ];
  homepage = "http://github.com/snoyberg/conduit";
  description = "Deterministic allocation and freeing of scarce resources";
  license = stdenv.lib.licenses.bsd3;
}
