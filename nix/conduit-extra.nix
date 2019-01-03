{ mkDerivation, async, attoparsec, base, blaze-builder, bytestring
, bytestring-builder, conduit, criterion, directory, exceptions
, filepath, hspec, monad-control, network, primitive, process
, QuickCheck, resourcet, stdenv, stm, streaming-commons, text
, transformers, transformers-base, typed-process, unliftio-core
}:
mkDerivation {
  pname = "conduit-extra";
  version = "1.2.3.2";
  sha256 = "1d5b66284703a4b9fb96a4c6a2213727208639871a675da9755e9a963fa230f6";
  revision = "1";
  editedCabalFile = "10smqv7xrjj5vhpdfj9ha89dm8nw04cv09avg58c6y8yybbz5i01";
  libraryHaskellDepends = [
    async attoparsec base blaze-builder bytestring conduit directory
    exceptions filepath monad-control network primitive process
    resourcet stm streaming-commons text transformers transformers-base
    typed-process unliftio-core
  ];
  testHaskellDepends = [
    async attoparsec base blaze-builder bytestring bytestring-builder
    conduit directory exceptions hspec process QuickCheck resourcet stm
    streaming-commons text transformers transformers-base
  ];
  benchmarkHaskellDepends = [
    base blaze-builder bytestring bytestring-builder conduit criterion
    transformers
  ];
  homepage = "http://github.com/snoyberg/conduit";
  description = "Batteries included conduit: adapters for common libraries";
  license = stdenv.lib.licenses.mit;
}
