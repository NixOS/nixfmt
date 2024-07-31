[
  (with b; c)
  (
    with b; # b
    c
  )
  (
    with # a
      b;
    c
  )
  (
    with # a
      b; # b
    c
  )
  (with b; cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc)
  (with b; cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc)
  { a = with b; 1; }
  { a = with b; 1 + 1; }
  {
    a = with b; {
      c = 1;
    };
  }
  {
    a = with b; {
      c = 1;
      d = 2;
      e = 3;
    };
  }
  {
    a =
      with b; # comment
      [
        1
        2
        3
      ];
  }
  {
    a =
      with b;
      # comment
      1;
  }
  {
    a = with b; 1;
    # comment
  }
  ([ 1 ])
  (with a; [ 1 ])
  ([
    1
    2
    3
  ])
  (with a; [
    1
    2
    3
  ])
  (with a; with b; with c; [ 1 ])
  (with a; with b; with c; { a = 1; })
  (
    with a; # comment
    with b;
    with c;
    {
      a = 1;
    }
  )
  (
    with a;
    with b;
    with c;
    {
      a = 1;
      b = 2;
    }
  )
  (
    with a; # comment
    with b;
    with c;
    {
      a = 1;
      b = 2;
    }
  )
  { a = with b; with b; with b; 1; }
  {
    binPath =
      with pkgs;
      makeBinPath ([
        rsync
        util-linux
      ]);
  }
  (with a; { })
  (with a; [
    1
    2
    3
  ])
  (with a; if null then true else false)
  (
    with a;
    let
    in
    [
      1
      2
      3
    ]
  )
  (
    {
      gst_plugins ? with gst_all_1; [
        gst-plugins-good
        gst-plugins-ugly
      ],
      more ?
        with stuff;
        let
        in
        [
          1
          2
          3
        ],
      things ? with a; if null then true else false,
      things ?
        with a;
        if null then true else "looooooooooooooooooooooooooooooooooooong",
    }:
    { }
  )
  {
    more =
      with stuff;
      let
      in
      [
        1
        2
        3
      ];
    things = with a; if null then true else false;
    things =
      with a;
      if null then true else "looooooooooooooooooooooooooooooooooooong";
  }
]
