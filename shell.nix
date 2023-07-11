{ nixpkgs ? import ./nixpkgs.nix {}
}:

with nixpkgs;

let ghc = haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      clash-ghc
      clash-prelude
      QuickCheck
      ghcid
      repa
      dense
    ]);
in
mkShell {
  name = "clash-exercises";
  buildInputs = [ ghc ];
}
