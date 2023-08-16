{ mkDerivation, base, lib, split
, vector
}:
mkDerivation {
  pname = "numskull";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base split vector ];
  testHaskellDepends = [
    base split vector
  ];
  license = lib.licenses.mit;
}