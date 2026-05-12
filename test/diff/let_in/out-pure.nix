let

  a =
    let
      b = 2;
      c = 3;
    in
    d;
  a =
    let
      c = 1;
    in
    f;

  a =
    let
      c = 1;
    in
    # e
    f;
  a =
    let
      c = 1; # d
    in
    f;

  a =
    let
      c = 1; # d
    in
    # e
    f;
  a =
    let # b
      c = 1;
    in
    f;
  a =
    let # b
      c = 1;
    in
    # e
    f;
  a =
    let # b
      c = 1; # d
    in
    f;
  a =
    let # b
      c = 1; # d
    in
    # e
    f;

  a =
    let
    in
    [ 1 2 ];

  a =
    let
      b = 0;

      # foo
      # bar
    in
    # baz
    # qux
    null;

  # Trailing comment on `in` with a long body must be idempotent
  a =
    let
    in
    # c
    f xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      g;

in

a
