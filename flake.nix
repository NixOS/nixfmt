# © 2019 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "A formatter for Nix code, intended to easily apply a uniform style.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-22.11";

    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-stable, flake-utils, ... }:
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
          overlays = [ overlay ];
        };

        pkgs-stable = import nixpkgs-stable {
          inherit system;
          overlays = [ overlay ];
        };

        inherit (pkgs) haskell lib;

        ghcjsPackages = pkgs-stable.haskell.packages.ghcjs810.override (old: {
          overrides = (self: super: {
            QuickCheck = haskell.lib.dontCheck super.QuickCheck;
            tasty-quickcheck = haskell.lib.dontCheck super.tasty-quickcheck;
            scientific = haskell.lib.dontCheck super.scientific;
            temporary = haskell.lib.dontCheck super.temporary;
            time-compat = haskell.lib.dontCheck super.time-compat;
          });
        });

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
          nixfmt-js = ghcjsPackages.callCabal2nix "nixfmt" src { };
          nixfmt-webdemo = pkgs.runCommandNoCC "nixfmt-webdemo" { } ''
            mkdir $out
            cp ${./js/index.html} $out/index.html
            cp ${./js/404.html} $out/404.html
            cp ${nixfmt-js}/bin/js-interface.jsexe/{rts,lib,out,runmain}.js $out
            substituteInPlace $out/index.html --replace ../dist/build/js-interface/js-interface.jsexe/ ./
          '';

          nixfmt-shell = nixfmt.env.overrideAttrs (oldAttrs: {
            buildInputs = oldAttrs.buildInputs ++ (with pkgs; [
              # nixfmt: expand
              cabal-install
              stylish-haskell
            ]);
          });

          inherit (pkgs) awscli reuse;
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.nixfmt-static}/bin/nixfmt";
        };

        devShells.default = self.packages.${system}.nixfmt-shell;
      });
}
