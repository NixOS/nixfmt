{ pkgs ? import <nixpkgs> { } }:

pkgs.haskellPackages.callPackage ./package.nix { }
