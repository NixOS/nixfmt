let
  foo = 1;
  "bar" = 2;
  ${"baz"} = 3;
  ${"in"} = 4;

in { inherit ${"foo"} bar "baz" "in"; }
