{ nixpkgs ? import ./nixpkgs.nix {} }:
nixpkgs.pkgs.haskellPackages.callPackage ./ndarray.nix { }
