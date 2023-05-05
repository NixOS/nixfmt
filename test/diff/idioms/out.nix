[
  {
    meta = with lib; {
      a = 1;
      b = 2;
      c = 3;
    };
  }

  {
    meta = with lib;
    # comment
      {
        a = 1;
        b = 2;
        c = 3;
      };
  }
]
