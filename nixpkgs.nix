{ sources ? import ./sources.nix }:

let clash-compiler = import sources.clash-compiler {};

    overlay = _: pkgs: {
        niv = (import sources.niv {}).niv;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            yoda =
              pkgs.haskell.lib.doJailbreak
                (self.callCabal2nix "yoda" sources.yoda {});
          } // clash-compiler;
        };
      };
in
import sources.nixpkgs
  { overlays = [ overlay ] ; config = { }; }
