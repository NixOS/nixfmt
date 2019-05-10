{ pkgs ? import <nixpkgs> { } }:
let nixfmt = pkgs.haskellPackages.callPackage ./package.nix { };
in (pkgs.haskell.lib.addBuildTool pkgs.cabal-install nixfmt).env
