[
  [
    {
      # multiline
      foo = "bar";
      foo2 = "barbar";
    }
  ]
  [
    (
      if foo then
        bar # multiline too
      else
        baz
    )
  ]
  [ 1 ]

  [ 1 ]

  [
    b
    d
  ]
  [
    b
    d # e
  ]
  [
    b # c
    d
  ]
  [
    b # c
    d # e
  ]
  [
    # a
    b
    d
  ]
  [
    # a
    b
    d # e
  ]
  [
    # a
    b # c
    d
  ]
  [
    # a
    b # c
    d # e
  ]

  [

    b

    d
  ]
  [

    # a

    b

    # c

    d

    # e
  ]

  [
    [
      multi
      line
    ]
  ]
  [ [ [ singleton ] ] ]
  [ [ [ { } ] ] ]
  [
    [
      [
        { }
        multiline
      ]
    ]
  ]
]
