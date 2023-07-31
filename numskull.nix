{ mkDerivation, base, doctest, hspec, lib, QuickCheck, split
, vector
}:
mkDerivation {
  pname = "numskull";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base split vector ];
  testHaskellDepends = [
    base doctest hspec QuickCheck split vector
  ];
  license = lib.licenses.mit;
}
