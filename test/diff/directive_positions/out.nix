{
  # Indented directive inside an attrset
  nested = {
    /*nixfmt:disable*/
    foo    =    1;
    bar    =    2;
    /*nixfmt:enable*/
    baz = 3;
  };

  # Directive inside a let binding
  letExample =
    let
    /*nixfmt:disable*/
    x    =    1;
    y    =    2;
    /*nixfmt:enable*/
      z = 3;
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
  trailingBracket = [
    # nixfmt:disable
    "x"
    "y"
    "z"
    /*nixfmt:enable*/
  ];

  # Trailing on {
  trailingBrace = {
    # nixfmt:disable
    c = 1;
    d = 2;
    /*nixfmt:enable*/
  };

  # Trailing on (
  trailingParen = (
    # nixfmt:disable
    1 + 2
  );

  # Trailing on in
  trailingIn =
    let
      x = 1;
    in
    # nixfmt:disable
    x;

  # -- Language annotation disambiguation --
  # /*nixfmt:disable*/ before a string must NOT be confused with a language annotation.

  # Directive before a string — ignored (trailing position), not a lang annotation
  notLangAnnotation = # nixfmt:disable
    "hello";

  # Real language annotation still works normally
  langAnnotation = /* lua */ "print(1)";

  # Directive on own line, followed by a language annotation inside disabled region
/*nixfmt:disable*/
  preservedLangAnnotation = /* lua */    "print(2)"   ;
/*nixfmt:enable*/

  # -- Around if/then/else and with --
  cond =
    if true then
/*nixfmt:disable*/
      { unformatted  =  1; }
/*nixfmt:enable*/
    else
      with pkgs;
/*nixfmt:disable*/
      [ a   b ]
/*nixfmt:enable*/
  ;

  # This MUST be formatted
  afterCond = 1;

  # -- Not on their own line --

  # Directive preceded on its line by the tail of a multi-line block comment:
  # not on its own line, so it is demoted to a plain comment and must not
  # open a disabled region
  /*
    multi
    line
  */
  # nixfmt:disable
  notDisabled = 1;

  # Same for a single-line block comment directly before the directive
  # c
  # nixfmt:disable
  alsoNotDisabled = 2;

  # A real region afterwards still works
/*nixfmt:disable*/
  disabled    =    3;
/*nixfmt:enable*/
  afterNotOwnLine = 4;

  # -- Function parameters --

  # Directives between attributes of a function parameter set
  paramsFn =
    {
      a
/*nixfmt:disable*/
, b    ? 3
, c
/*nixfmt:enable*/
      ,
      d,
    }:
    a;

  # -- Member check chains --

  # Disabled region around a chained member check is preserved byte-exactly
/*nixfmt:disable*/
  chainDisabled = a    ?   b   ?   c;
/*nixfmt:enable*/

  # Directive sharing a line inside a chain is inert (demoted to a plain comment)
  chainInert =
    a
      ? b # nixfmt:disable
      ? c;
  chainInertAfterQmark =
    a
      # nixfmt:disable
      ? b
      ? c;

  # Region opening and closing between fallbacks.
  # The unformatted region affects the formatted part of the chain: its hard
  # line breaks force the whole chain to expand.
  chainRegionBetween =
    x
      ? a
/*nixfmt:disable*/
    ?   b   ?   c
/*nixfmt:enable*/
      ? d;

  # Region opening mid-chain and closing after the binding.
  # Same as above: unformatted region affecting the formatted part of the chain.
  chainRegionAcrossEnd =
    x
      ? a
/*nixfmt:disable*/
    ?   b   ?   c   ;
/*nixfmt:enable*/

  # Region opening mid-chain: the part before the directive is still formatted.
  # Same as above (unformatted region affecting the formatted part), here
  # leaving a dangling question mark line.
  chainRegionTail =
    x
      ? a
      ?
/*nixfmt:disable*/
    b   ?   c;
/*nixfmt:enable*/
  afterChainRegion = 1;

  # -- Directly after a string token --

  # The directive must not affect the string preceding it:
  # it is still converted to a simple string.
  afterString = [
    "convert me"
    /*nixfmt:disable*/
    "frozen"    "strings"
    /*nixfmt:enable*/
  ];
}
