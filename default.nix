# © 2019 Serokell <hi@serokell.io>
# © Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import <nixpkgs> { }
, installOnly ? true
}:

let
  nixfmt = pkgs.haskellPackages.callCabal2nix "nixfmt" ./. {};

in
  if installOnly
  then pkgs.haskell.lib.justStaticExecutables nixfmt
  else nixfmt
