let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) haskell;
  ghcjsPackages = haskell.packages.ghcjs86.override (old: {
    overrides = (self: super: {
      QuickCheck = haskell.lib.dontCheck super.QuickCheck;
      tasty-quickcheck = haskell.lib.dontCheck super.tasty-quickcheck;
    });
  });
in rec {
  nixfmt = pkgs.haskellPackages.callCabal2nix "nixfmt" ./. { };
  nixfmt-js = ghcjsPackages.callCabal2nix "nixfmt" ./. { };
  web-demo = pkgs.runCommandNoCC "nixfmt-webdemo" {} ''
    mkdir $out
    cp ${./js/index.html} $_/index.html
    cp ${nixfmt-js}/bin/js-interface.jsexe/{rts,lib,out,runmain}.js $out
    substituteInPlace $out/index.html --replace ../dist/build/js-interface/js-interface.jsexe/ ./
  '';
  # $(nix-build release.nix -A update-gh-pages)/bin/nixfmt-upd-gh-pages && git push origin gh-pages
  update-gh-pages = pkgs.writeShellScriptBin "nixfmt-upd-gh-pages" ''
    REV=$(git rev-parse --short HEAD)
    git worktree add gh-pages gh-pages
    cp ${web-demo}/* gh-pages
    cd gh-pages
    git add .
    git commit -am "Update to $REV"
    cd ..
    git worktree remove --force gh-pages
    git worktree prune
  '';
}
