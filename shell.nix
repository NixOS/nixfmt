{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv;
  ghc = pkgs.haskellPackages.ghcWithHoogle (hpkgs: with hpkgs; [
    megaparsec
    prettyprinter
    text
  ]);

in stdenv.mkDerivation {
  name = "nixfmt";
  buildInputs = with pkgs; [
    cabal-install
    ghc
  ];
}
