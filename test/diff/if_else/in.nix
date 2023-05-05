[
  (if true
  then {
    version = "1.2.3";
  }
  else {
    version = "3.2.1";
  })
  (if true
  then ''
    some text
  ''
  else ''
    other text
  '')
  (if ./a then b else c)
  (if /**/ a /**/ then /**/ b /**/ else /**/ c)
  (if # test
  a # test
  then # test
  b # test
  else # test
  c)
  (if # test
  /**/
  a # test
  /**/
  then # test
  b # test
  /**/
  else # test
  /**/
  c)
  (if if a then b else c then b else if a then b else if a then b else c)
  (if if a then b else c then b else if a then b else /*x*/ if a then b else c)
  (if
    (if
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c))
    then
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c))
    else
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c)))
  then
    (if
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c))
    then
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c))
    else
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c)))
  else
    (if
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c))
    then
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c))
    else
      (if
        (if a then b else c)
      then
        (if a then b else c)
      else
        (if a then b else c))))
]
