let
  sources = import ./npins;
in
{
  system ? builtins.currentSystem,
  nixpkgs ? sources.nixpkgs,
  serokell-nix ? sources.serokell-nix,
}:
let
  overlay = self: super: {
    haskell = super.haskell // {
      packageOverrides = self: super: { nixfmt = self.callCabal2nix "nixfmt" haskellSource { }; };
    };

    treefmt = super.treefmt.overrideAttrs (old: {
      patches = [
        # Makes it work in parallel: https://github.com/numtide/treefmt/pull/282
        (self.fetchpatch {
          url = "https://github.com/numtide/treefmt/commit/f596795cd24b50f048cc395866bb90a89d99152d.patch";
          hash = "sha256-EPn+JAT3aZLSWmpdi9ULZ8o8RvrX+UFp0cQWfBcQgVg=";
        })
      ];
    });
  };

  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      overlay
      (import (serokell-nix + "/overlay"))
    ];
    config = { };
  };

  inherit (pkgs) haskell lib;
  fs = lib.fileset;

  allFiles = fs.gitTracked ./.;

  # Used for source-wide checks
  source = fs.toSource {
    root = ./.;
    fileset = allFiles;
  };

  haskellSource = fs.toSource {
    root = ./.;
    # Limit to only files needed for the Haskell build
    fileset = fs.intersection allFiles (
      fs.unions [
        ./nixfmt.cabal
        ./src
        ./main
        ./LICENSE
      ]
    );
  };

  build = lib.pipe pkgs.haskellPackages.nixfmt [
    haskell.lib.justStaticExecutables
    haskell.lib.dontHaddock
    (drv: lib.lazyDerivation { derivation = drv; })
  ];

  treefmtEval = (import sources.treefmt-nix).evalModule pkgs {
    # Used to find the project root
    projectRootFile = ".git/config";

    # This uses the version from Nixpkgs instead of the local one,
    # which would require building the package to get a development shell
    programs.nixfmt-rfc-style.enable = true;
    # We don't want to format the files we use to test the formatter!
    settings.formatter.nixfmt-rfc-style.excludes = [ "test/*" ];

    # Haskell formatter
    programs.fourmolu.enable = true;
  };

  checks = {
    inherit build;
    hlint = pkgs.build.haskell.hlint haskellSource;
    reuse = pkgs.stdenvNoCC.mkDerivation {
      name = "nixfmt-reuse";
      src = source;
      nativeBuildInputs = with pkgs; [ reuse ];
      buildPhase = "reuse lint";
      installPhase = "touch $out";
    };
    tests = pkgs.stdenvNoCC.mkDerivation {
      name = "nixfmt-tests";
      src = fs.toSource {
        root = ./.;
        fileset = fs.intersection allFiles ./test;
      };
      nativeBuildInputs = with pkgs; [
        shellcheck
        build
        gitMinimal
      ];
      patchPhase = "patchShebangs .";
      buildPhase = ''
        export HOME=$(mktemp -d)
        export PAGER=cat
        git config --global user.name "Test"
        git config --global user.email "test@test.com"
        git config --global init.defaultBranch main
        ./test/test.sh
        ./test/mergetool.sh
      '';
      installPhase = "touch $out";
    };
    treefmt = treefmtEval.config.build.check source;
  };
in
build
// {
  packages.nixfmt = build;

  inherit pkgs;

  shell = pkgs.haskellPackages.shellFor {
    packages = p: [ p.nixfmt ];
    nativeBuildInputs = with pkgs; [
      cabal-install
      stylish-haskell
      haskellPackages.haskell-language-server
      shellcheck
      npins
      hlint
      treefmtEval.config.build.wrapper
    ];
  };

  inherit checks;

  ci = pkgs.linkFarm "ci" checks;
}
