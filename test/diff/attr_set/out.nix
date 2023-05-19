[
  { }
  { # a
  }
  { a = 1; }
  { a = 1; }

  { b = 1; }
  {
    b = 1; # c
  }
  { # a
    b = 1;
  }
  { # a
    b = 1; # c
  }

  rec { c = 1; }
  rec {
    c = 1; # d
  }
  rec { # b
    c = 1;
  }
  rec { # b
    c = 1; # d
  }
  rec # a
  {
    c = 1;
  }
  rec # a
  {
    c = 1; # d
  }
  rec # a
  { # b
    c = 1;
  }
  rec # a
  { # b
    c = 1; # d
  }

  {
    a = rec {
      a = {
        a = rec {
          a = {
            a = rec { a = { a = rec { a = { a = rec { a = { }; }; }; }; }; };
          };
        };
      };
    };
  }

  rec {

    c = 1;

    e = 1;

  }

  rec
  # a
  {

    # b

    c = 1;

      # d

    e = 1;

      # f

  }
  {
    x =
      {
        foo = 1;
        bar = 2;
          # multiline
      }
      .${x};
    y = # more multiline
      {
        foo = 1;
        bar = 2;
          # multiline
      }
      .${x};
    z = functionCall {
      # multi
      #line
    } [
      # several
      items
    ];
    a =
      [
        some
        flags # multiline
      ]
      ++ [ short ]
      ++ [
        more
        stuff # multiline
      ]
      ++ (if foo then
        [ bar ]
      else
        [ baz ])
      ++ [ ]
      ++ (optionals condition [
        more
        items
      ])
      ;
    b = with pkgs; [
      a
      lot
      of
      packages
    ];
  }
]
