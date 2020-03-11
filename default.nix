# © 2020 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import ./nix, installOnly ? true }:

let
  inherit (pkgs) lib;
  regexes = [ ".*.cabal$" "^src.*" "^main.*" "^Setup.hs$" "^js.*" "LICENSE" ];
  src = builtins.path {
    path = ./.;
    name = "nixfmt-src";
    filter = path: type:
      let relPath = lib.removePrefix (toString ./. + "/") (toString path);
      in lib.any (re: builtins.match re relPath != null) regexes;
  };
  nixfmt-pkgs =
    with pkgs;
    haskell-nix.mkStackPkgSet {
      stack-pkgs = {
        extras = hackage:{
          nixfmt = import (haskell-nix.callCabalToNix {
            name = "nixfmt";
            inherit src;
          });
          stm = hackage.stm."2.5.0.0".revisions.default;
          ghcjs-base = hackage.ghcjs-base."0.2.0.0".revisions.default;
        };
        resolver = if pkgs.targetPlatform.isGhcjs then "lts-14.27" else "lts-15.3";
        modules = lib.optionals pkgs.targetPlatform.isGhcjs [ ./ghcjs ];
      };
    };
  nixfmt = nixfmt-pkgs.config.hsPkgs.nixfmt;

in if installOnly then
  nixfmt.components.exes.nixfmt
else
  nixfmt.components.all
