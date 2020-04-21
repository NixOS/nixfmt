# © 2019 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

{ sources ? import nix/sources.nix
, nixpkgs ? sources.nixpkgs }:

let
  overlay = self: super: {
    haskell = super.haskell // {
      packageOverrides = self: super: {
        nixfmt = self.callCabal2nix "nixfmt" src { };
      };
    };
  };

  pkgs = import nixpkgs { overlays = [ overlay ]; };

  inherit (pkgs) haskell lib;

  ghcjsPackages = haskell.packages.ghcjs86.override (old: {
    overrides = (self: super: {
      QuickCheck = haskell.lib.dontCheck super.QuickCheck;
      tasty-quickcheck = haskell.lib.dontCheck super.tasty-quickcheck;
      scientific = haskell.lib.dontCheck super.scientific;
      temporary = haskell.lib.dontCheck super.temporary;
      time-compat = haskell.lib.dontCheck super.time-compat;
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
  nixfmt = pkgs.haskellPackages.nixfmt;
  nixfmt-static = haskell.lib.justStaticExecutables nixfmt;
  nixfmt-deriver = nixfmt-static.cabal2nixDeriver;
  nixfmt-js = ghcjsPackages.callCabal2nix "nixfmt" src { };
  nixfmt-webdemo = pkgs.runCommandNoCC "nixfmt-webdemo" { } ''
    mkdir $out
    cp ${./js/index.html} $out/index.html
    cp ${./js/404.html} $out/404.html
    cp ${nixfmt-js}/bin/js-interface.jsexe/{rts,lib,out,runmain}.js $out
    substituteInPlace $out/index.html --replace ../dist/build/js-interface/js-interface.jsexe/ ./
  '';

  nixfmt-shell = nixfmt.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ (with pkgs; [ # nixfmt: expand
      cabal-install
      stylish-haskell
    ]);
  });

  inherit (pkgs) awscli;
}
