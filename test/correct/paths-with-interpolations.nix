let
  foo = "foo";
  bar = "bar";

in
[ /${foo} ./${foo} /foo/${bar} /foo${bar} /${foo}/bar /${foo}bar /foo${bar}baz ]
