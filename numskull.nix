{ mkDerivation, base, containers, criterion, deepseq, hmatrix
, hspec, lib, massiv, mwc-random, parsec, QuickCheck, split
, template-haskell, vector
}:
mkDerivation {
  pname = "numskull";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers deepseq parsec split template-haskell vector
  ];
  executableHaskellDepends = [
    base criterion deepseq hmatrix massiv mwc-random vector
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = lib.licenses.mit;
  mainProgram = "bench";
}
