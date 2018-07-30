with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "nix-format";

  buildInputs = [
    cabal-install
    haskell.compiler.ghc843
    wget
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${zlib}/lib:$LD_LIBRARY_PATH
  '';
}
