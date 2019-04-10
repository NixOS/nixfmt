{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv;
  ghc = haskellPackages.ghcWithHoogle (hpkgs: with hpkgs; [
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
