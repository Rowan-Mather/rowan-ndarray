{ nixpkgs ? import nix/nixpkgs.nix {} }:
(import ./default.nix { inherit nixpkgs; }).env.overrideAttrs (finalAttrs: prevAttrs: {
  buildInputs = with nixpkgs.haskellPackages; prevAttrs.buildInputs ++ [
    haskell-language-server
    ghcid.bin
  ];
})
