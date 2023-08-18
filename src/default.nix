let
  pkgs = import ../ihaskell/pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  nixpkgs = import pkgs.nixpkgs {};
  compiler = "ghc902";
  packages = self: with self; [];
}
