[
  (a b)
  ((a b) (a b)
    (a # b
      c)
    ( # a
      b # c
      d # e
    ))
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
    name1 = function arg { asdf = 1; };

    name2 = function arg { asdf = 1; } argument;

    name3 = function arg { asdf = 1; } { qwer = 12345; } argument;
  }
  {
    name1 = function arg { asdf = 1; };

    name2 = function arg
      {
        asdf = 1;
          # multiline
      }
      argument;

    name3 = function arg
      {
        asdf = 1;
          # multiline
      }
      { qwer = 12345; }
      argument;
  }
  {
    name4 = function arg { asdf = 1; }
      {
        qwer = 12345;
        qwer2 = 54321;
      }
      argument;
  }
  {
    option1 = function arg { asdf = 1; }
      {
        qwer = 12345;
        qwer2 = 54321;
      }
      lastArg;

    option2 = function arg { asdf = 1; }
      {
        qwer = 12345;
        qwer2 = 54321;
      }
      lastArg;

    option3 = function arg { asdf = 1; }
      {
        qwer = 12345;
        qwer2 = 54321;
      }
      lastArg;
  }
  # https://github.com/kamadorueda/alejandra/issues/372#issuecomment-1435083516
  {
    outputs =
      {
        utils,
      }:
      # For each supported platform,
      utils.lib.eachDefaultSystem (system: { })
      ;
  }
  {
    escapeSingleline = libStr.escape [
      "\\"
      ''"''
      "\${"
    ];
    escapeMultiline = libStr.replaceStrings
      [
        "\${"
        "''"
      ]
      [
        "''\${"
        "'''"
      ];
    test = foo
      [ # multiline
        1
        2
        3
      ]
      [ ]
      { }
      [ ]
      [
        1
        2
        3 # multiline
      ];
    looooooooong =
      (toINI
        { inherit mkSectionName mkKeyValue listsAsDuplicateKeys aaaaaaaa; }
        sections);
    looooooooong' = toINI
      { inherit mkSectionName mkKeyValue listsAsDuplicateKeys aaaaaaaa; }
      sections;
  }
]
