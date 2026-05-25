let
  sources = import ./npins;
in
# NOTE: update mirrored args in `default.nix` when modifying
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

  haskellBuildPipeline = [
    haskell.lib.justStaticExecutables
    haskell.lib.dontHaddock
    (drv: lib.lazyDerivation { derivation = drv; })
  ];

  build = lib.pipe pkgs.haskellPackages.nixfmt haskellBuildPipeline;
  buildStatic = lib.pipe pkgs.pkgsStatic.haskellPackages.nixfmt haskellBuildPipeline;

  treefmtEval = (import sources.treefmt-nix).evalModule pkgs {
    # Used to find the project root
    projectRootFile = ".git/config";

    # This uses the version from Nixpkgs instead of the local one,
    # which would require building the package to get a development shell
    programs.nixfmt.enable = true;
    # We don't want to format the files we use to test the formatter!
    settings.formatter.nixfmt.excludes = [ "test/*" ];

    # Haskell formatter
    programs.fourmolu.enable = true;
  };

  checks = {
    inherit build;
    cabal-check = pkgs.stdenvNoCC.mkDerivation {
      name = "nixfmt-cabal-check";
      src = source;
      nativeBuildInputs = with pkgs; [
        cabal-install
      ];
      buildPhase = lib.escapeShellArgs [
        "cabal"
        "check"
        # Ignore some warnings about missing dependency bounds. We don't bother
        # specifying upper bounds, as we're not a Haskell
        # library for others to consume. We get reproducibility by virtue of
        # using the nixpkgs Haskell package set. When we update nixpkgs, we
        # rely upon our tests to tell us if anything broke.
        "--ignore=missing-upper-bounds"
        "--ignore=missing-bounds-important"
        # We enable `werror` by default. This is OK because we only support one
        # version of GHC at a time.
        "--ignore=werror"
      ];
      installPhase = "touch $out";
    };
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
{
  packages = {
    nixfmt = build;
    nixfmt-static = buildStatic;
  };

  inherit pkgs lib;

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

  treefmt = treefmtEval.config.build.wrapper;
}
