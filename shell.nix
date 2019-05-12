# © 2019 Serokell <hi@serokell.io>
# © Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import <nixpkgs> { } }:

(import ./. { inherit pkgs; installOnly = false; }).env.overrideAttrs
  (oldAttrs: { buildInputs = oldAttrs.buildInputs ++ [pkgs.cabal-install]; })
