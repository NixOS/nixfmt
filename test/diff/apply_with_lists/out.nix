# This file contains an assortment of test cases involving list-heavy function calls

[
  (f [ ] [ rhs lhs ])
  (lib.mkMerge [ false false ])
  (replaceStrings
    [ "\${" "''" ]
    #force multiline
    [ "''\${" "'''" ]
  )
  (replaceStrings [ ''"'' "\\" ] [ ''\"'' "\\\\" ] name)
  (replaceStrings
    [ ''"'' "\\" ]
    # force multiline
    [ ''\"'' "\\\\" ]
    name
  )
  (replaceStrings [ "@" ":" "\\" "[" "]" ] [ "-" "-" "-" "" "" ])
  (lists.removePrefix [ 1 2 ] [ ])
  (lists.removePrefix aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    [ 1 2 ]
    [ ]
  )
  (builtins.replaceStrings [ "@NIX_STORE_VERITY@" ] [ partitionTypes.usr-verity ]
    (builtins.readFile ./assert_uki_repart_match.py)
  )
  (replaceStrings [ "-" ] [ "_" ] (toUpper final.rust.cargoShortTarget))
  (lib.mkChangedOptionModule
    [ "security" "acme" "validMin" ]
    [ "security" "acme" "defaults" "validMinDays" ]
    (config: config.security.acme.validMin / (24 * 3600))
  )
  (lib.replaceStrings [ "https://registry" ".io/providers" ] [ "registry" ".io" ]
    homepage
  )
  (lib.mkRenamedOptionModule [ "boot" "extraTTYs" ] [ "console" "extraTTYs" ])
  # This line is engineered to exactly hit the line length limit
  (lib.mkRenamedOptionModule
    [ "hardware" "package234" ]
    [ "hardware" "graphics" ]
  )
  (mkRenamedOptionModule
    [
      "services"
      "xserver"
      "displayManager"
      "sddm"
      "enable"
    ]
    [ "services" "displayManager" "sddm" "enable" ]
  )
  (map (buildAllowCommand "allow" [ "snapshot" "mount" "destroy" ]))
  (map (x: "${x} ${escapeShellArgs [ stateDir workDir logsDir ]}") [
    "+${unconfigureRunner}" # runs as root
    configureRunner
    setupWorkDir
  ])
  (lib.checkListOfEnum "${pname}: theme accent"
    [
      "Blue"
      "Flamingo"
      "Green"
    ]
    [ accent ]
    lib.checkListOfEnum
    "${pname}: color variant"
    [ "Latte" "Frappe" "Macchiato" "Mocha" ]
    [ variant ]
  )
  (lib.switch
    [ coq.coq-version ssreflect.version ]
    [
      {
        cases = [
          (lib.versions.range "8.15" "8.20")
          lib.pred.true
        ];
        out = "2.0.4";
      }
      {
        cases = [
          "8.5"
          lib.pred.true
        ];
        out = "20170512";
      }
    ]
    null
  )
  # https://github.com/NixOS/nixfmt/issues/268 regression test. [] and {} should be treated the same
  (defaultNullOpts.mkNullable (
    with types; either str (listOf str)
  ) [ ] "Some example long text that causes the line to be too long.")
  (defaultNullOpts.mkNullable (
    with types; either str (listOf str)
  ) { } "Some example long text that causes the line to be too long.")
]
