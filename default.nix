# © 2019 Serokell <hi@serokell.io>
# © Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import <nixpkgs> { } }:

pkgs.haskellPackages.callCabal2nix "nixfmt" ./. { }
