{
  # Indented directive inside an attrset
  nested = {
    /*nixfmt:disable*/
    foo    =    1;
    bar    =    2;
    /*nixfmt:enable*/
    baz   =   3;
  };

  # Directive inside a let binding
  letExample = let
    /*nixfmt:disable*/
    x    =    1;
    y    =    2;
    /*nixfmt:enable*/
    z   =   3;
  in
    x;

  # Directive after a comment block
  # some comment
  /*nixfmt:disable*/
  commented    =    1;
  /*nixfmt:enable*/

  # Directive right after opening brace (own line)
  afterBrace = {
    /*nixfmt:disable*/
    a    =    1;
    b    =    2;
    /*nixfmt:enable*/
  };

  # Directive right after opening bracket (own line)
  afterBracket = [
    /*nixfmt:disable*/
    "a"    "b"    "c"
    /*nixfmt:enable*/
  ];

  # -- Trailing position --
  # Directives sharing a line with other tokens are not on their own line,
  # so they are treated as regular comments and ignored.

  # Trailing on [
  trailingBracket = [  /*nixfmt:disable*/
    "x"    "y"    "z"
    /*nixfmt:enable*/
  ];

  # Trailing on {
  trailingBrace = {  /*nixfmt:disable*/
    c    =    1;
    d    =    2;
    /*nixfmt:enable*/
  };

  # Trailing on (
  trailingParen = (  /*nixfmt:disable*/
    1   +   2
  );

  # Trailing on in
  trailingIn = let
    x = 1;
  in  /*nixfmt:disable*/
    x      ;

  # -- Language annotation disambiguation --
  # /*nixfmt:disable*/ before a string must NOT be confused with a language annotation.

  # Directive before a string — ignored (trailing position), not a lang annotation
  notLangAnnotation = /*nixfmt:disable*/ "hello";

  # Real language annotation still works normally
  langAnnotation = /* lua */ "print(1)";

  # Directive on own line, followed by a language annotation inside disabled region
/*nixfmt:disable*/
  preservedLangAnnotation = /* lua */    "print(2)"   ;
/*nixfmt:enable*/
}
