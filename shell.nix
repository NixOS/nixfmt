# Copyright Serokell OU <hi@serokell.io>
# Copyright Lars Jellema <lars.jellema@gmail.com>
# SPDX-License-Identifier: MPL-2.0

{ pkgs ? import <nixpkgs> { } }:

(import ./. { inherit pkgs; }).env.overrideAttrs
  (oldAttrs: { buildInputs = oldAttrs.buildInputs ++ [pkgs.cabal-install]; })
