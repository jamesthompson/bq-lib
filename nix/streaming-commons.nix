{ mkDerivation, array, async, base, blaze-builder, bytestring
, deepseq, directory, gauge, hspec, network, process, QuickCheck
, random, stdenv, stm, text, transformers, unix, zlib
}:
mkDerivation {
  pname = "streaming-commons";
  version = "0.1.19";
  sha256 = "43fcae90df5548d9968b31371f13ec7271df86ac34a484c094616867ed4217a7";
  libraryHaskellDepends = [
    array async base blaze-builder bytestring directory network process
    random stm text transformers unix zlib
  ];
  testHaskellDepends = [
    array async base blaze-builder bytestring deepseq hspec network
    QuickCheck text unix zlib
  ];
  benchmarkHaskellDepends = [
    base blaze-builder bytestring deepseq gauge text
  ];
  homepage = "https://github.com/fpco/streaming-commons";
  description = "Common lower-level functions needed by various streaming data libraries";
  license = stdenv.lib.licenses.mit;
}
