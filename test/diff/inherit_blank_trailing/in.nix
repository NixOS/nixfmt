[
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
    inherit
      a # mixed trivialities

      # comment 1
      # comment 2

      # comment 3 after blanks
      b # multiple newlines


      c # multiple comments
      # comment 1
      # comment 2
      # comment 3
      ;
  }
]
