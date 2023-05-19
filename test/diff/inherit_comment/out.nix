{
  inherit # eeby deeby
    a
    # b
    c
    ;

    # https://github.com/kamadorueda/alejandra/issues/372
  inherit
    (
      pkgs.haskell.lib
    )
    # doJailbreak - remove package bounds from build-depends of a package
    doJailbreak
    # dontCheck - skip tests
    dontCheck
    # override deps of a package
    # see what can be overriden - https://github.com/NixOS/nixpkgs/blob/0ba44a03f620806a2558a699dba143e6cf9858db/pkgs/development/haskell-modules/generic-builder.nix#L13
    overrideCabal
    ;
}
