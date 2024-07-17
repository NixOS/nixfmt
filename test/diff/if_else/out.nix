[
  (
    if true then
      {
        version = "1.2.3";
      }
    else
      {
        version = "3.2.1";
      }
  )
  (
    if true then
      ''
        some text
      ''
    else
      ''
        other text
      ''
  )
  (if ./a then b else c)
  (if a then b else c)
  (
    # test
    if
      a # test
    then # test
      b # test
    # test
    else
      c
  )
  (
    # test
    if
      a # test
    then # test
      b # test
    # test
    else
      c
  )
  (
    if
      [
        multiline
        # tmp
        condition
      ]
    then
      foo
    else if
      [
        more
        multi
        line
      ]
    then
      bar
    else
      baz
  )
  (
    if
      unabsorbable # comment
      == multiline
    then
      foo
    else if
      unabsorbable # comment
      == multiline
    then
      bar
    else
      baz
  )
  (
    if if a then b else c then
      b
    else if a then
      b
    else if a then
      b
    else
      c
  )
  (
    if if a then b else c then
      b
    else if a then
      b
    # x
    else if a then
      b
    else
      c
  )
  (
    if
      (
        if
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
        then
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
        else
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
      )
    then
      (
        if
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
        then
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
        else
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
      )
    else
      (
        if
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
        then
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
        else
          (if (if a then b else c) then (if a then b else c) else (if a then b else c))
      )
  )
]
