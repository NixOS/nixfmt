{
  # Region 1
/*nixfmt:disable*/
  foo    =    1;
  bar    =    2;
/*nixfmt:enable*/

  # Formatted normally between regions
  baz   =   3;

  # Region 2 (adjacent — enable immediately followed by disable)
/*nixfmt:disable*/
  qux    =    4;
  quux   =    5;
/*nixfmt:enable*/

  # -- False positives section --
  # All of these contain directive-like text that must NOT be recognized.
  # Crucially, they are NOT inside a real disable region, so if the scanner
  # mistakenly treats them as real directives, subsequent code would break.

  # Multi-line string with a lone disable (no enable) — must not start a region
  falsePositive1 = ''
    /*nixfmt:disable*/
    this   is   a   string
  '';

  # This MUST be formatted (proves the string above didn't start a disabled region)
  formatted1   =   100;

  # Multi-line string with '' escape sequences near directive-like text.
  # Only disable (no enable) — if the scanner misreads this, the rest of the file breaks.
  falsePositive2 = ''
    here's some escaped quotes: '''
    /*nixfmt:disable*/
    and an escaped interpolation ''${foo}
    more text
  '';

  # This MUST be formatted
  formatted2   =   200;

  # Directive inside a regular string spanning multiple lines
  falsePositive3 = "
/*nixfmt:disable*/
  not   a   directive
";

  # This MUST be formatted
  formatted3   =   300;

  # -- Ignored directives --
  # Directives that share a line with other code must be ignored.

  # Trailing on a code line — just a comment, not a directive
  ignored1   =   1; /*nixfmt:disable*/
  ignored2   =   2;

  # Directive with extra text after */ — not a valid directive
  /*nixfmt:disable*/ ignoredInline   =   3;

  # -- Consecutive directives --
  # Consecutive disables: second one is ignored (already disabled)
/*nixfmt:disable*/
/*nixfmt:disable*/
  alpha    =    10;
  beta     =    20;
/*nixfmt:enable*/

  # Consecutive enables: second one is ignored (already enabled)
/*nixfmt:disable*/
  gamma    =    30;
/*nixfmt:enable*/
/*nixfmt:enable*/

  # An empty region: both directives are kept, nothing is disabled
/*nixfmt:disable*/
/*nixfmt:enable*/
  emptyRegion   =   40;

  # -- Near-miss spellings --
  # None of these are directives; they are normalized like any other comment.
  /* nixfmt:disable */
  /*nixfmt:disable */
  /*NIXFMT:DISABLE*/
  /*nixfmt:off*/
  # /*nixfmt:disable*/
  notASingleDirectiveAbove   =   5;

  # This MUST be formatted
  last   =   6;
}
