let
  inherit lib;
in
[
  ({ }: null)
  (
    {
    }:
    null
  )
  (
    {

    }:
    null
  )

  (
    { lib }:
    let
      foo = 1;
    in
    foo
  )
  (
    /*
      Collection of functions useful for debugging
      Some comment
    */
    { lib }:
    let
      foo = 1;
    in
    foo
  )
  (
    a: b: # c
    d
  )
  (
    { }:
    b: # c
    d
  )
  (
    a:
    { }: # c
    d
  )
  (a: d)
  (
    a: # c
    d
  )
  (
    a # b
    :
    d
  )
  (
    a # b
    : # c
    d
  )
  (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
  (
    {
      pkgs ? import ./.. { },
      locationsXml,
    }:
    null
  )
  (
    a: b: c:
    { }:
    a: b: c:
    a
  )

  (
    { pkgs, ... }:
    {
      # Stuff
    }
  )

  (
    { pkgs, ... }:
    let
    in
    pkgs
  )

  (
    a:
    {
      b,
      ...
    }:
    c: {
      # Stuff
    }
  )

  (
    a:
    {
      b,
      c,
      ...
    }:
    d: {
      # Stuff
    }
  )

  (
    {
      gst_plugins ? [
        gst-plugins-good
        gst-plugins-ugly
      ],
      more ?
        let
        in
        [ 1 2 3 ],
      things ? if null then true else false,
      things ?
        if null then true else "loooooooooooooooooooooooooooooooooooooooooooong",
      more ? (
        let
        in
        [ 1 2 3 ]
      ),
      foo ? (with bar; [ 1 2 3 ]),
      foo ? (
        with bar;
        let
        in
        [ 1 2 3 ]
      ),
      things ? (if null then true else false),
      things ? (
        if null then true else "loooooooooooooooooooooooooooooooooooooooooooong"
      ),
      things ? (
        if null then [ 1 2 3 ] else "loooooooooooooooooooooooooooooooooooooooooooong"
      ),
      things ? # comment
        (if null then [ 1 2 3 ] else "loooooooooooooooooooooooooooooooooooooooooooong"),
    }:
    { }
  )
  {
    a =
      name: with config.ids; ''
        --nodaemon --syslog --prefix=${name} --pidfile /run/${name}/${name}.pid ${name}
      '';
    a' = name: ''
      --nodaemon --syslog --prefix=${name} --pidfile /run/${name}/${name}.pid ${name}
    '';
    b =
      p: with p; [
        ConfigIniFiles
        FileSlurp
      ];
    b' = p: [
      ConfigIniFiles
      FileSlurp
    ];
    mkUrls =
      {
        name,
        version,
        biocVersion,
      }:
      [ "mirror://bioc/${biocVersion}/data/experiment/${name}_${version}.tar.gz" ];
    c =
      { ... }:
      {
        foo = true;
      };
    c = { ... }: [ 1 ];
    d =
      { a }:
      {
        foo = true;
      };
    d = { a }: [ 1 ];
    e =
      { a, b }:
      {
        foo = true;
      };
    e = { a, b }: [ 1 ];
  }
]
