{ pkgs ? import <nixpkgs> {} }:

(pkgs.haskellPackages.callPackage ./package.nix {}).env.overrideAttrs (
  oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [ pkgs.cabal-install ];
  }
)
