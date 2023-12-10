[
  (
    # Function call with comment
    mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
  )
  # Function call with comment
  (mapAttrsToStringsSep "\n" mkSection attrsOfAttrs)
  (
    # Function call with comment
    mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
  )

  # Same song again, but within function application

  (foo bar baz (
    # Function call with comment
    mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
  ))
  (foo bar baz
    # Function call with comment
    (
      mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
    )
  )
  (foo bar baz (
    # Function call with comment
    mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
  ))

  # And again, but with wide function application

  (foo
    [
      1
      2 # multiline
    ]
    baz
    (
      # Function call with comment
      mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
    )
  )
  (foo
    [
      1
      2 # multiline
    ]
    bar
    baz
    # Function call with comment
    (
      mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
    )
  )
  (foo
    [
      1
      2 # multiline
    ]
    bar
    baz
    (
      # Function call with comment
      mapAttrsToStringsSep "\n" mkSection attrsOfAttrs
    )
  )

  # Now in attribute set position
  {
    a =
      # Function call with comment
      mapAttrsToStringsSep "\n" mkSection attrsOfAttrs;
    b = # Function call with comment
      mapAttrsToStringsSep "\n" mkSection attrsOfAttrs;
  }
  [
    (mapAttrsToStringsSep
      [
        force
        long
      ]
      "\n"
      mkSection
      attrsOfAttrs
    )
  ]
  (a b)
  ((a b) (a b)
    (
      # b
      a c
    )
    (
      # a
      # c
      b d # e
    )
  )
  ''
    otherModules=${
      pkgs.writeText "other-modules.json" (
        l.toJSON (
          l.mapAttrs
            (
              pname: subOutputs:
              let
                pkg = subOutputs.packages."${pname}".overrideAttrs (
                  old: {
                    buildScript = "true";
                    installMethod = "copy";
                  }
                );
              in
              "${pkg}/lib/node_modules/${pname}/node_modules"
            )
            outputs.subPackages
        )
      )
    }
  ''
  {
    name1 = function arg {asdf = 1;};

    name2 = function arg {asdf = 1;} argument;

    name3 = function arg {asdf = 1;} {qwer = 12345;} argument;
  }
  {
    name1 = function arg {asdf = 1;};

    name2 =
      function arg
        {
          asdf = 1;
          # multiline
        }
        argument;

    name3 =
      function arg
        {
          asdf = 1;
          # multiline
        }
        {qwer = 12345;}
        argument;
  }
  {
    name4 =
      function arg {asdf = 1;}
        {
          qwer = 12345;
          qwer2 = 54321;
        }
        argument;
  }
  {
    option1 =
      function arg {asdf = 1;}
        {
          qwer = 12345;
          qwer2 = 54321;
        }
        lastArg;

    option2 =
      function arg {asdf = 1;}
        {
          qwer = 12345;
          qwer2 = 54321;
        }
        lastArg;

    option3 =
      function arg {asdf = 1;}
        {
          qwer = 12345;
          qwer2 = 54321;
        }
        lastArg;
  }
  # https://github.com/kamadorueda/alejandra/issues/372#issuecomment-1435083516
  {
    outputs =
      {utils}:
      # For each supported platform,
      utils.lib.eachDefaultSystem (system: {});
  }
  {
    escapeSingleline = libStr.escape [
      "\\"
      ''"''
      "\${"
    ];
    escapeMultiline =
      libStr.replaceStrings
        [
          "\${"
          "''"
        ]
        [
          "''\${"
          "'''"
        ];
    test =
      foo
        [
          # multiline
          1
          2
          3
        ]
        []
        {}
        []
        [
          1
          2
          3 # multiline
        ];
    looooooooong =
      (toINI
        {
          inherit
            mkSectionName
            mkKeyValue
            listsAsDuplicateKeys
            aaaaaaaa
            ;
        }
        sections
      );
    looooooooong' =
      toINI
        {
          inherit
            mkSectionName
            mkKeyValue
            listsAsDuplicateKeys
            aaaaaaaa
            ;
        }
        sections;
  }

  # Test breakup behavior at different line lengths
  {
    name = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name_ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name__ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name___ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name____ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name_____ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name______ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name_______ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name________ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name_________ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name__________ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name___________ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name____________ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name_____________ = sanitizeDerivationName (builtins.substring 33 (-1) (path'));
    name______________ = sanitizeDerivationName (
      builtins.substring 33 (-1) (path')
    );
    name_______________ = sanitizeDerivationName (
      builtins.substring 33 (-1) (path')
    );
    name________________ = sanitizeDerivationName (
      builtins.substring 33 (-1) (path')
    );
    name_________________ = sanitizeDerivationName (
      builtins.substring 33 (-1) (path')
    );
    name__________________ = sanitizeDerivationName (
      builtins.substring 33 (-1) (path')
    );
  }
  # Same but without binders
  [
    (sanitizeDerivationName (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName_ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName__ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName___ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName____ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName_____ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName______ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName_______ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName________ (builtins.substring 33 (-1) (baseNameOf path')))
    (sanitizeDerivationName_________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName__________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName___________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName____________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName_____________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName______________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName_______________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName________________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName_________________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
    (sanitizeDerivationName__________________ (
      builtins.substring 33 (-1) (baseNameOf path')
    ))
  ]
  # Function calls with lambdas as last argument
  {
    overrideArgs = copyArgs (newArgs: makeOverridable f (overrideWith newArgs));
    overrideArgs_ = copyArgs (newArgs: makeOverridable f (overrideWith newArgs));
    overrideArgs__ = copyArgs (newArgs: makeOverridable f (overrideWith newArgs));
    overrideArgs___ = copyArgs (newArgs: makeOverridable f (overrideWith newArgs));
    overrideArgs____ = copyArgs (newArgs: makeOverridable f (overrideWith newArgs));
    # Get a list of suggested argument names for a given missing one
    getSuggestions =
      arg:
      lib.pipe (autoArgs // args) [
        lib.attrNames
        # Only use ones that are at most 2 edits away. While mork would work,
        # levenshteinAtMost is only fast for 2 or less.
        (lib.filter (lib.strings.levenshteinAtMost 2 arg))
        # Put strings with shorter distance first
        (lib.sort (x: y: lib.strings.levenshtein x arg < lib.strings.levenshtein y arg))
        # Only take the first couple results
        (lib.take 3)
        # Quote all entries
        (map (x: ''"'' + x + ''"''))
      ];
  }
  # Function calls with multiline functions
  {
    foo =
      (callPackage ../generic-builders/manifest.nix {
        # A lot of values here
      }).overrideAttrs
        (
          prevAttrs: {
            # stuff here
          }
        );
    # Variant with a selection on the function without parentheses
    foo2 =
      {
        # A lot of values here
      }
      .overrideAttrs
        (
          prevAttrs: {
            # stuff here
          }
        );
    # Also test within parenthesized function instead of just attribute sets
    foo3 =
      (
        (callPackage ../generic-builders/manifest.nix {
          # A lot of values here
        }).overrideAttrs
        stuff
        (
          prevAttrs: {
            # stuff here
          }
        )
      );
    # Add a comment at a bad place
    foo4 =
      (
        # comment
        (callPackage ../generic-builders/manifest.nix {
          # A lot of values here
        }).overrideAttrs
          stuff
          (
            prevAttrs: {
              # stuff here
            }
          )
      );
  }
]
