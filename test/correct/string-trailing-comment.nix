# A comment trailing the closing delimiter of a string is demoted to its own line
# only when a token could be reindented onto the comment's column on the very
# next line. That needs both a column set by the string content rather than by
# the indentation, and no blank line in between. These must all round-trip.
[
  # A blank line before the next item keeps the comment trailing.
  "
" # a

  1

  # A blank line before a standalone comment keeps it trailing too.
  "
" # b

  # standalone
  2

  # A blank line before an operator operand keeps it trailing.
  (
    "
" # d

    + ""
  )

  # An uncut '' string closes at a column set by the indentation, so nothing can
  # be reindented onto its trailing comment and no blank line is needed.
  ''
    a
    b
  '' # e
  3
]
