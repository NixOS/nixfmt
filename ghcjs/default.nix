{ pkgs, ... }: {
  bootPkgs = [ "ghcjs-prim" ];
  nonReinstallablePkgs = [
    "Cabal" "array" "base" "binary" "bytestring"
    "containers" "deepseq" "directory" "filepath"
    "ghc" "ghc-boot" "ghc-boot-th" "ghc-compact"
    "ghc-heap" "ghc-prim" "ghci" "ghcjs-prim"
    "ghcjs-th" "integer-gmp" "mtl" "parsec" "pretty"
    "process" "rts" "template-haskell" "text" "time"
    "transformers" "unix"
    
    "hpc" "hsc2hs"
    
    # we can't build this one, so let's pretend it pre-exists.
    "terminfo"
    
    # This one is just absolutely broken.
    "cabal-doctest"
  ];
  packages.network.configureFlags =
    [ "--configure-option=--host=x86_64-linux" ];
  packages.haskeline.flags.terminfo = false;
  configureFlags = [ "--ghc-option=-dedupe" ];
  setup-depends = [ (import ./Cabal-3.0.0.0.nix pkgs.buildPackages).hsPkgs.Cabal ];
}
