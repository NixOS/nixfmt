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

  regexes = [
    ".*.cabal$"
    "^src.*"
    "^main.*"
    "^Setup.hs$"
    "^js.*"
    "LICENSE"
  ];
  src = builtins.path {
    path = ./.;
    name = "nixfmt-src";
    filter =
      path: type:
      let
        relPath = lib.removePrefix (toString ./. + "/") (toString path);
      in
      lib.any (re: builtins.match re relPath != null) regexes;
  };

  build = pkgs.haskellPackages.nixfmt;
in
build
// rec {
  packages = {
    nixfmt = build;
    nixfmt-static = haskell.lib.justStaticExecutables packages.nixfmt;
    nixfmt-deriver = packages.nixfmt-static.cabal2nixDeriver;

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
