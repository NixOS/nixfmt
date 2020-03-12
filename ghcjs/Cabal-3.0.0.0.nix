pkgs:
pkgs.haskell-nix.hackage-project {
    name = "cabal-install";
    version = "3.0.0.0";
    modules = [
      {
        packages.Cabal.patches = [
          ./Cabal-3.0.0.0-js-input-exes.diff
          ./Cabal-3.0.0.0-drop-pkg-db-check.diff
          ./Cabal-3.0.0.0-no-final-checks.diff
        ];
      }
      {
        nonReinstallablePkgs = [
          "array" "base" "binary" "bytestring" "containers" "deepseq" "directory"
          "filepath" "ghc" "ghc-boot" "ghc-boot-th" "ghc-compact" "ghc-heap"
          "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp" "libiserv"
          "mtl" "parsec" "pretty" "process" "rts"
          "stm" "template-haskell" "terminfo" "text" "time"
          "transformers" "unix" "xhtml"
        ];
      }
    ];
  }
