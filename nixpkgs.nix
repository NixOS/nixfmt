import (fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/b5f5c97f7d67a99b67731a8cfd3926f163c11857.tar.gz";
  sha256 = "1m9xb3z3jxh0xirdnik11z4hw95bzdz7a4p3ab7y392345jk1wgm";
}) { config = {}; overlays = [
       (self: super: {
         haskell = super.haskell // {
           packages = super.haskell.packages // {
             ghc864 = super.haskell.packages.ghc864.extend (hself: hsuper: {
               happy = super.haskell.lib.dontCheck (hsuper.callHackage "happy" "1.19.9" {});
             });
           };
         };
       })
     ]; }
