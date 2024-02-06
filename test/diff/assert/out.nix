[
  (
    assert b;
    e
  )
  (
    assert b; # d
    e
  )
  (
    assert b # c
    ;
    e
  )
  (
    assert b # c
    ; # d
    e
  )
  (
    # a
    assert b;
    e
  )
  (
    # a
    assert b; # d
    e
  )
  (
    # a
    assert b # c
    ;
    e
  )
  (
    # a
    assert b # c
    ; # d
    e
  )
  (
    assert b;
    cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  )
  (
    assert b;
    cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  )
  (
    assert
      let
        int = a: if a then 1 else 0;
        xor = a: b: ((builtins.bitXor (int a) (int b)) == 1);
      in
      lib.assertMsg (xor (gitRelease != null) (officialRelease != null)) (
        "must specify `gitRelease` or `officialRelease`"
        + (lib.optionalString (gitRelease != null) " — not both")
      );
    assert if true then 1 else 0;
    assert
      if true then # multiline
        1
      else
        0;
    assert
      with lib.strings;
      (versionAtLeast stdenv.cc.version "7.1" && versionOlder stdenv.cc.version "13");
    assert (
      stringLength (drvName (toString oldDependency))
      == stringLength (drvName (toString newDependency))
    );
    assert (
      lib.assertMsg (!enableGoldPlugin)
        "Gold plugin cannot be enabled on LLVM16 due to a upstream issue: https://github.com/llvm/llvm-project/issues/61350"
    );
    assert lib.assertMsg (!enableGoldPlugin)
      "Gold plugin cannot be enabled on LLVM16 due to a upstream issue: https://github.com/llvm/llvm-project/issues/61350";
    assert (
      builtins.length eriAm == eriDeriv + 1
      && builtins.foldl' (a: b: a && b) true (
        builtins.map (a: a <= maxAm && a >= 0) eriAm
      )
    );
    assert assertMsg (originalValid -> absConcatOrig == absConcatNormalised)
      "For valid subpath \"${str}\", appending to an absolute Nix path value gives \"${absConcatOrig}\", but appending the normalised result \"${tryOnce.value}\" gives a different value \"${absConcatNormalised}\"";
    assert lib.assertMsg (strw <= width)
      "fixedWidthString: requested string length (${toString width}) must not be shorter than actual length (${toString strw})";
    assert lib.foldl (
      pass: { assertion, message }: if assertion final then pass else throw message
    ) true (final.parsed.abi.assertions or [ ]);
    assert
      getErrors {
        nixpkgs.localSystem = pkgs.stdenv.hostPlatform;
        nixpkgs.hostPlatform = pkgs.stdenv.hostPlatform;
        nixpkgs.pkgs = pkgs;
      } == [ ];
    [ ]
  )
]
