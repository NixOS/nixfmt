[
  (
    {
      foo,
      bar,
      # Some comment
      baz,
    }:
    { }
  )
  (
    {
      foo,
      bar, # Some comment
      baz, # More comment
    }:
    { }
  )
  (
    {
      foo,
      bar,
      # Some comment
      baz,
    }:
    { }
  )
  (
    {
      foo,
      bar, # Some comment
    }:
    { }
  )
  (
    a@{
      self,
      gomod2nix,
      mach-nix,
    }:
    _
  )
  (
    {
      self,
      gomod2nix,
      mach-nix,
    }@inp:
    _
  )
  (
    {
      a ? [ 1 2 3 ],
      b ? {
        # ...
      },
    }:
    _
  )
  ({ }: _)
  ({ a }: _)
  ({ }: _)
  ({ ... }: _)
  ({ ... }: _)
  ({ ... }: _)
  ({ ... }: _)

  ({ b, e, ... }: _)
  (
    {
      b,
      e,
      ... # h
    }:
    _
  )
  (
    {
      b,
      e, # g
      ...
    }:
    _
  )
  (
    {
      b,
      e, # g
      ... # h
    }:
    _
  )
  (
    {
      b,
      e, # f
      ...
    }:
    _
  )
  (
    {
      b,
      e, # f
      ... # h
    }:
    _
  )
  (
    {
      b,
      e # f
      , # g
      ...
    }:
    _
  )
  (
    {
      b,
      e # f
      , # g
      ... # h
    }:
    _
  )
  (
    {
      b, # d
      e,
      ...
    }:
    _
  )
  (
    {
      b, # d
      e,
      ... # h
    }:
    _
  )
  (
    {
      b, # d
      e, # g
      ...
    }:
    _
  )
  (
    {
      b, # d
      e, # g
      ... # h
    }:
    _
  )
  (
    {
      b, # d
      e, # f
      ...
    }:
    _
  )
  (
    {
      b, # d
      e, # f
      ... # h
    }:
    _
  )
  (
    {
      b, # d
      e # f
      , # g
      ...
    }:
    _
  )
  (
    {
      b, # d
      e # f
      , # g
      ... # h
    }:
    _
  )
  (
    {
      b, # c
      e,
      ...
    }:
    _
  )
  (
    {
      b, # c
      e,
      ... # h
    }:
    _
  )
  (
    {
      b, # c
      e, # g
      ...
    }:
    _
  )
  (
    {
      b, # c
      e, # g
      ... # h
    }:
    _
  )
  (
    {
      b, # c
      e, # f
      ...
    }:
    _
  )
  (
    {
      b, # c
      e, # f
      ... # h
    }:
    _
  )
  (
    {
      b, # c
      e # f
      , # g
      ...
    }:
    _
  )
  (
    {
      b, # c
      e # f
      , # g
      ... # h
    }:
    _
  )
  (
    {
      b # c
      , # d
      e,
      ...
    }:
    _
  )
  (
    {
      b # c
      , # d
      e,
      ... # h
    }:
    _
  )
  (
    {
      b # c
      , # d
      e, # g
      ...
    }:
    _
  )
  (
    {
      b # c
      , # d
      e, # g
      ... # h
    }:
    _
  )
  (
    {
      b # c
      , # d
      e, # f
      ...
    }:
    _
  )
  (
    {
      b # c
      , # d
      e, # f
      ... # h
    }:
    _
  )
  (
    {
      b # c
      , # d
      e # f
      , # g
      ...
    }:
    _
  )
  (
    {
      b # c
      , # d
      e # f
      , # g
      ... # h
    }:
    _
  )
  (
    # a
    { b, e, ... }: _
  )
  (
    # a
    {
      b,
      e,
      ... # h
    }:
    _
  )
  (
    # a
    {
      b,
      e, # g
      ...
    }:
    _
  )
  (
    # a
    {
      b,
      e, # g
      ... # h
    }:
    _
  )
  (
    # a
    {
      b,
      e, # f
      ...
    }:
    _
  )
  (
    # a
    {
      b,
      e, # f
      ... # h
    }:
    _
  )
  (
    # a
    {
      b,
      e # f
      , # g
      ...
    }:
    _
  )
  (
    # a
    {
      b,
      e # f
      , # g
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # d
      e,
      ...
    }:
    _
  )
  (
    # a
    {
      b, # d
      e,
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # d
      e, # g
      ...
    }:
    _
  )
  (
    # a
    {
      b, # d
      e, # g
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # d
      e, # f
      ...
    }:
    _
  )
  (
    # a
    {
      b, # d
      e, # f
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # d
      e # f
      , # g
      ...
    }:
    _
  )
  (
    # a
    {
      b, # d
      e # f
      , # g
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # c
      e,
      ...
    }:
    _
  )
  (
    # a
    {
      b, # c
      e,
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # c
      e, # g
      ...
    }:
    _
  )
  (
    # a
    {
      b, # c
      e, # g
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # c
      e, # f
      ...
    }:
    _
  )
  (
    # a
    {
      b, # c
      e, # f
      ... # h
    }:
    _
  )
  (
    # a
    {
      b, # c
      e # f
      , # g
      ...
    }:
    _
  )
  (
    # a
    {
      b, # c
      e # f
      , # g
      ... # h
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e,
      ...
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e,
      ... # h
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e, # g
      ...
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e, # g
      ... # h
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e, # f
      ...
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e, # f
      ... # h
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e # f
      , # g
      ...
    }:
    _
  )
  (
    # a
    {
      b # c
      , # d
      e # f
      , # g
      ... # h
    }:
    _
  )

  (
    {
      a ? null,
    }:
    _
  )
  (
    # a
    {
      b  # a
        ?  # a
          null # c
      , # d
      e  # a
        ?  # a
          null # f
      , # g
      ... # h
    }:
    _
  )

  (
    {
      # a
      #
      b
        # a
        #
        ?
          # a
          #
          null,
      # c
      #
      # d
      #
      e
        # a
        #
        ?
          # a
          #
          null,
      # f
      #
      # g
      #
      ...
    # h
    #
    }
    # i
    #
    :
    # j
    #
    _
  )
]
