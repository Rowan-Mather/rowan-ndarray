let
  pkgs = import ../pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" rec { 
  nixpkgs = import pkgs.nixpkgs {};
  compiler = "ghc902";
  packages = self: with self; [
    (import ../../default.nix 
      { inherit nixpkgs; })
  ];
}
