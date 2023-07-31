{ sources ? import ./sources.nix }:

let overlay = _: pkgs: {
  niv = (import sources.niv {}).niv;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      dense = pkgs.haskell.lib.markUnbroken (
        pkgs.haskell.lib.dontCheck ( # doctests are broken
          pkgs.haskell.lib.overrideSrc super.dense {
            src = sources.dense;
          }
        )
      );
    };
  };
};
in
import sources.nixpkgs { overlays = [ overlay ] ; config = {}; }