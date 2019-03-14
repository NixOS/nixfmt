{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv;
  hpkgs = pkgs.haskell.packages.ghc863;
  ghc = hpkgs.ghcWithHoogle (hpkgs: with hpkgs; [
    megaparsec
    prettyprinter
    text
  ]);

in stdenv.mkDerivation {
  name = "nixfmt";
  src = ./.;
  buildInputs = with pkgs; [
    cabal-install
    ghc
  ];
}
