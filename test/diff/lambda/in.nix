let
  inherit lib;
in
[
  (
    { lib, }:
    let
      foo = 1;
    in
    foo
  )
  (
    /* Collection of functions useful for debugging
      Some comment */
    { lib }:
    let
      foo = 1;
    in
    foo
  )
  (a: b: /*c*/ d)
  ({}: b: /*c*/ d)
  (a: {}: /*c*/ d)
  (a       :       d)
  (a       : /*c*/ d)
  (a /*b*/ :       d)
  (a /*b*/ : /*c*/ d)
  (
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  )
  (
    aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa )
  ({ pkgs ? import ./.. { }, locationsXml }: null)
  (a: b: c:
    { }:
    a: b: c:
    a)

  ({pkgs, ...}: {
    # Stuff
  })

  ({pkgs, ...}: let
  in pkgs)

  (a: {b,
  ...}: c: {
    # Stuff
  })

  (a: {b, c,
  ...}: d: {
    # Stuff
  })
]
