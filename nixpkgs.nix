{ sources ? import ./sources.nix }:

let clash-compiler = import sources.clash-compiler {};

    overlay = _: pkgs: {
        niv = (import sources.niv {}).niv;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            dense = pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.overrideSrc super.dense {
              src = sources.dense;
            });
            yoda =
              pkgs.haskell.lib.doJailbreak
                (self.callCabal2nix "yoda" sources.yoda {});
          } // clash-compiler;
        };
      };
in
import sources.nixpkgs
  { overlays = [ overlay ] ; config = { }; }
