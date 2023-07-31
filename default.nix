{ nixpkgs ? import nix/nixpkgs.nix {} }:
nixpkgs.pkgs.haskellPackages.callPackage ./numskull.nix { }
