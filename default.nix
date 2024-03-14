# © 2019 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
# © 2024 Silvan Mosberger <contact@infinisil.com>
#
# SPDX-License-Identifier: MPL-2.0

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
      packageOverrides = self: super: { nixfmt = self.callCabal2nix "nixfmt" src { }; };
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

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./nixfmt.cabal
      ./src
      ./main
      ./LICENSE
    ];
  };

  build = lib.pipe pkgs.haskellPackages.nixfmt [
    haskell.lib.justStaticExecutables
    haskell.lib.dontHaddock
    (drv: lib.lazyDerivation { derivation = drv; })
  ];
in
build
// rec {
  packages = {
    nixfmt = build;
    nixfmt-deriver = build.cabal2nixDeriver;

    nixfmt-shell = packages.nixfmt.env.overrideAttrs (oldAttrs: {
      buildInputs =
        oldAttrs.buildInputs
        ++ (with pkgs; [
          # nixfmt: expand
          cabal-install
          stylish-haskell
          shellcheck
          npins
        ]);
    });

    inherit (pkgs) reuse;
  };

  shell = packages.nixfmt-shell;

  checks = {
    hlint = pkgs.build.haskell.hlint ./.;
    stylish-haskell = pkgs.build.haskell.stylish-haskell ./.;
  };
}
