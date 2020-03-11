# © 2020 Serokell <hi@serokell.io>
# © 2019 Lars Jellema <lars.jellema@gmail.com>
#
# SPDX-License-Identifier: MPL-2.0

let
  pkgs = import ./nix;
  inherit (pkgs) lib;
in rec {
  nixfmt = import ./default.nix { inherit pkgs; installOnly = false; };
  nixfmt-static = import ./default.nix { inherit pkgs; installOnly = true; };
  # todo: missing js-sources
  nixfmt-js = import ./default.nix { installOnly = false; pkgs = pkgs.pkgsCross.ghcjs; };
  web-demo = pkgs.runCommandNoCC "nixfmt-webdemo" {} ''
    mkdir $out
    cp ${./js/index.html} $_/index.html
    cp ${nixfmt-js}/bin/js-interface.jsexe/{rts,lib,out,runmain}.js $out
    substituteInPlace $out/index.html --replace ../dist/build/js-interface/js-interface.jsexe/ ./
  '';
}
