{ pkgs ? import <nixpkgs> { } }:

(import ./. { inherit pkgs; }).env.overrideAttrs
  (oldAttrs: { buildInputs = oldAttrs.buildInputs ++ [pkgs.cabal-install]; })
