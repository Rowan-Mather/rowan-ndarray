{ mkDerivation, base, dense, lib }:
mkDerivation {
  pname = "ndarray";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base dense ];
  license = lib.licenses.mit;
}
