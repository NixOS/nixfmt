- `inherit`s can occur between `let` and `in`. Indentation should be based on
  being between certain tokens in certain nodes, not on being a SetEntry.
- Maybe when a block is indented twice with regard to its surrounding lines, the
  indentation should be reduced to a single level. This will cause reindenting
  of unrelated lines in a commit if at a later time, one of the intermediate
  indentation levels needs to be used. The problem is best avoided by giving
  every opening and closing paren its own line if everything doesn't fit on a
  single line.
- Comments before the closing paren of a list or set should be indented.
- The trailing trivia of the last node after its first newline should be
  formatted as if they were leading trivia.
- Remove newlines from the start of the first token in the file
- Use `format_trivia` for the first element and closing token of every list and
  set that spans multiple lines or doesn't fit on a single line.
- Consider giving every node a meta.
- Add indentation and decide on formatting rules for if-then-else.
- Add indentation and decide on formatting rules for multiline expressions.
- Add indentation and decide on formatting rules for multiline
  `{ matching lambdas }:`.
- Add double newlines after every line containing a set or list closing token
  and no such token on the next line?
- Insert space before the second and third child of every `Operator`.
- Fix spaces in a lot of other places. Always put spaces in the leading trivia.
  The trailing trivia should be empty or a space followed by a comment. Do this
  before `format_all_trivia` so that it will be replaced with a newline when
  needed.
