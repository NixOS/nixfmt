# © 2019 Serokell <hi@serokell.io>
# © Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import <nixpkgs> { }, installOnly ? true }:

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
  nixfmt = pkgs.haskellPackages.callCabal2nix "nixfmt" src { };

in if installOnly then
  pkgs.haskell.lib.justStaticExecutables nixfmt
else
  nixfmt
