[
  {
    # empty inherit o.O
    inherit ;
    inherit aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa;
  }
  {
    inherit
      a
      b
      c
      d
      e
      f
      g
      h
      i
      j
      ;
  }
  { inherit aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa; }
  { inherit b d; }
  {
    inherit
      b
      d # e
      ;
  }
  {
    inherit
      b # c
      d
      ;
  }
  {
    inherit
      b # c
      d # e
      ;
  }
  {
    inherit # a
      b
      d
      ;
  }
  {
    inherit # a
      b
      d # e
      ;
  }
  {
    inherit # a
      b # c
      d
      ;
  }
  {
    inherit # a
      b # c
      d # e
      ;
  }
  {
    inherit # test
      a # test

      b # test
      c # test
      d # test

      e
      f

      g
      h
      ;
  }

  {
    inherit ;
    inherit a;
    inherit a;
  }
]
