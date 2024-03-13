# © 2019 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "A formatter for Nix code, intended to easily apply a uniform style.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    serokell-nix = {
      url = "github:serokell/serokell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, serokell-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: super: {
          haskell = super.haskell // {
            packageOverrides = self: super: {
              nixfmt = self.callCabal2nix "nixfmt" src { };
            };
          };
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay serokell-nix.overlay ];
        };

        inherit (pkgs) haskell lib;

        regexes =
          [ ".*.cabal$" "^src.*" "^main.*" "^Setup.hs$" "^js.*" "LICENSE" ];
        src = builtins.path {
          path = ./.;
          name = "nixfmt-src";
          filter = path: type:
            let relPath = lib.removePrefix (toString ./. + "/") (toString path);
            in lib.any (re: builtins.match re relPath != null) regexes;
        };

      in {
        packages = rec {
          default = nixfmt;
          nixfmt = pkgs.haskellPackages.nixfmt;
          nixfmt-static = haskell.lib.justStaticExecutables nixfmt;
          nixfmt-deriver = nixfmt-static.cabal2nixDeriver;

          nixfmt-shell = nixfmt.env.overrideAttrs (oldAttrs: {
            buildInputs = oldAttrs.buildInputs ++ (with pkgs; [
              # nixfmt: expand
              cabal-install
              stylish-haskell
              shellcheck
            ]);
          });

          inherit (pkgs) reuse;
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.nixfmt-static}/bin/nixfmt";
        };

        devShells.default = self.packages.${system}.nixfmt-shell;

        checks = {
          hlint = pkgs.build.haskell.hlint ./.;
          stylish-haskell = pkgs.build.haskell.stylish-haskell ./.;
        };
      });
}
