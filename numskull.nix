{ mkDerivation, base, containers, deepseq, hspec, lib, parsec
, QuickCheck, split, template-haskell, vector
}:
mkDerivation {
  pname = "numskull";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq parsec split template-haskell vector
  ];
  testHaskellDepends = [ base hspec QuickCheck ];
  license = lib.licenses.mit;
}
