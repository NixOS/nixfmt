{
  # A region entirely inside one interpolation: the code around it is
  # formatted and the string is reindented as usual.
  selfContained = ''
    a ${
/*nixfmt:disable*/
      1   +   1
/*nixfmt:enable*/
    } b
  '';

  # This MUST be formatted
  afterSelfContained = 1;

  # A region entirely inside one interpolation: the code around it is
  # formatted and the string is reformatted as usual.
  selfContainedWithDiff = ''
    The first interpolation should be formatted
    a ${
      toString [
        "a"
        "b"
        "c"
      ]
    } ${
/*nixfmt:disable*/
      1   +   1
/*nixfmt:enable*/
    } b
  '';

  # An unclosed disable inside an interpolation extends to the rest of the
  # file. The string it cuts through keeps all its lines at their original
  # columns (reindenting only part of a string would change its value and
  # break idempotency; found by fuzzing, see Mic92/nixfmt-rs#89).
  cutTail = ''
    text ${
/*nixfmt:disable*/
      2
    }
tail
  '';
  frozen   =   2;
/*nixfmt:enable*/

  # This MUST be formatted
  afterCutTail = 3;

  # A region may also end inside an interpolation; the string head that it
  # covers is reproduced verbatim and the rest keeps its original columns.
/*nixfmt:disable*/
  frozenHead   =   4;
  cutHead = ''
    head ${
/*nixfmt:enable*/
      5 + 5
    }
    tail text
  '';

  # This MUST be formatted
  afterCutHead = 6;

  # A cut string that would otherwise be converted to a "simple" string must
  # keep its '' delimiters, since the raw region contains the original syntax.
/*nixfmt:disable*/
  convertible    =    ''${
  /*nixfmt:enable*/
    x
  }'';

  # This MUST be formatted
  afterConvertible = 7;

  # A region balanced inside one interpolation does NOT cut the string: the
  # raw region is pure interpolation code, so the string still converts to a
  # "simple" string, exactly as it would without the directives.
  balancedConvertible = "${
/*nixfmt:disable*/
    f   x   y
/*nixfmt:enable*/
  }";

  # A region entering through an interpolation and leaving past the string's
  # end cuts it: the raw region contains the closing '', so no conversion.
  cutPastEnd = ''${
/*nixfmt:disable*/
    f   x   y
  }'';
/*nixfmt:enable*/

  # This MUST be formatted
  afterBalanced = 11;

  # A region crossing two interpolations of the same string
  crossInterp = ''
    a ${
/*nixfmt:disable*/
      8
    } frozen   text ${
/*nixfmt:enable*/
      9
    } c
  '';

  # This MUST be formatted
  last = 10;

  # -- More interpolation shapes --

  # A cut simple (double-quoted) string: content lines are already emitted
  # verbatim, so only the code layout matters
  cutSimple = "${
/*nixfmt:disable*/
    1
  } rest
text";
  frozenSimple   =   1;
/*nixfmt:enable*/

  # This MUST be formatted
  afterCutSimple = 2;

  # A region escaping two string levels at once (a simple string inside the
  # interpolation of an indented string): both strings are cut
  nested = ''
    x ${"inner ${
/*nixfmt:disable*/
      1
    } tail" } y
  '';
  frozenNested   =   3;
/*nixfmt:enable*/

  # This MUST be formatted
  afterNested = 4;

  # A region boundary inside an attr-path interpolation
  ${
/*nixfmt:disable*/
    key
  } = 1;
  frozenSelector   =   5;
/*nixfmt:enable*/

  # This MUST be formatted
  afterSelector = 6;

  # Two self-contained regions in different interpolations of one string:
  # the string is not cut and is reindented normally
  twoRegions = ''
    a ${
/*nixfmt:disable*/
      1
/*nixfmt:enable*/
    } b ${
/*nixfmt:disable*/
      2
/*nixfmt:enable*/
    } c
  '';

  # This MUST be formatted
  afterTwoRegions = 7;
}
