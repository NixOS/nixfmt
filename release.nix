# © 2019 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

let
  pkgs = import ./nixpkgs.nix;
  inherit (pkgs) haskell lib;
  ghcjsPackages = haskell.packages.ghcjs86.override (old: {
    overrides = (self: super: {
      QuickCheck = haskell.lib.dontCheck super.QuickCheck;
      tasty-quickcheck = haskell.lib.dontCheck super.tasty-quickcheck;
      scientific = haskell.lib.dontCheck super.scientific;
    });
  });
  regexes = [ ".*.cabal$" "^src.*" "^main.*" "^Setup.hs$" "^js.*" "LICENSE" ];
  src = builtins.path {
    path = ./.;
    name = "nixfmt-src";
    filter = path: type:
      let relPath = lib.removePrefix (toString ./. + "/") (toString path);
      in lib.any (re: builtins.match re relPath != null) regexes;
  };
in rec {
  nixfmt = pkgs.haskellPackages.callCabal2nix "nixfmt" src { };
  nixfmt-static = haskell.lib.justStaticExecutables nixfmt;
  nixfmt-js = ghcjsPackages.callCabal2nix "nixfmt" src { };
  web-demo = pkgs.runCommandNoCC "nixfmt-webdemo" {} ''
    mkdir $out
    cp ${./js/index.html} $_/index.html
    cp ${nixfmt-js}/bin/js-interface.jsexe/{rts,lib,out,runmain}.js $out
    substituteInPlace $out/index.html --replace ../dist/build/js-interface/js-interface.jsexe/ ./
  '';
}
