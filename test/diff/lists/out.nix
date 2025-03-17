[
  [ ]
  [
  ]
  [

  ]

  [
    [

    ]
  ]

  [
    "string"

  ]

  [
    {
      # multiline
      foo = "bar";
      foo2 = "barbar";
    }
  ]
  {
    # List in attrset with comment

    imports0 = [ ];

    imports2 = [
      # ./disko.nix
      ./hardware-configuration.nix
    ];
    imports3 = [
      # comment
      ./disko.nix
      ./hardware-configuration.nix
    ];
  }
  [
    (
      if foo then
        bar # multiline too
      else
        baz
    )
  ]
  [ 1 ]

  [
    1
  ]

  [
    b
    d
  ]
  [
    b
    d # e
  ]
  [
    b # c
    d
  ]
  [
    b # c
    d # e
  ]
  [
    # a
    b
    d
  ]
  [
    # a
    b
    d # e
  ]
  [
    # a
    b # c
    d
  ]
  [
    # a
    b # c
    d # e
  ]

  [

    b

    d

  ]
  [

    # a

    b

    # c

    d

    # e

  ]

  [
    [
      multi
      line
    ]
  ]
  [ [ [ singleton ] ] ]
  [ [ [ { } ] ] ]
  [
    [
      [
        { }
        multiline
      ]
    ]
  ]
]

  # Regression https://github.com/NixOS/nixfmt/issues/228
  {
    one = [
      "hello"
      "beautiful"
    ]
    ++ lib.optionals true [
      (x ++ [ bash ])
      "wonderful"
      "world"
    ];

    many = [
      "hello"
      "beautiful"
    ]
    ++ lib.optionals true [
      (x ++ [ bash ])
      "wonderful"
      "world"
    ]
    ++ lib.optionals true [ ]
    ++ [ ];

    boot.kernelParams =
      [ aaaaaaaaaaaaaa ]
      ++ optionals config.boot.vesa [
        "vga=0x317"
        "nomodeset"
      ];

    foo = [ bar ] ++ baz;
  }
