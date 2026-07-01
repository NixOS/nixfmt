{
  # Directive in string content (not an interpolation) — must be ignored.
  # Only disable (no enable) — if the scanner misreads this, the rest of the file breaks.
  stringContent = ''
    content
    /*nixfmt:disable*/
    this is just a string
    more content
  '';

  # This MUST be formatted (proves the string directive was ignored)
  afterString   =   1;

  # Code inside interpolation is formatted normally
  withInterpolation = ''
    text
    ${
       1
         + 2
    }
  '';

  # -- Regular (double-quoted) strings --

  # Directive inside regular string content must be ignored.
  # Only disable (no enable) — if the scanner misreads this, the rest of the file breaks.
  regularStringContent = "
    /*nixfmt:disable*/
    not a directive
  ";

  # This MUST be formatted
  afterRegularString   =   10;

  # Regular string with interpolation
  regularInterp = "hello ${   1   +   2   } world";

  # -- Nested interpolations --

  # Regular string inside indented string interpolation
  nestedRegularInMulti = ''
    prefix ${   "inner   ${   1   +   2   }   value"   } suffix
  '';

  # Indented string inside regular string interpolation
  nestedMultiInRegular = "${   ''
    multi
    line
    content
  ''   +   "x"   }";

  # Deeply nested: string in interpolation in string in interpolation
  deeplyNested = ''
    a ${ "b ${ ''
      c ${   1   +   2   }
    ''   }" }
  '';

  # This MUST be formatted
  afterNested   =   20;

  # -- Directives with nested string interpolations --

  # Directive inside a nested interpolation (real Nix code context)
  directiveInNestedInterp = ''
    outer ${
      "inner ${
        ''
          deep ${
/*nixfmt:disable*/
            1    +    2    +    3
/*nixfmt:enable*/
          }
        ''
      }"
    }
  '';

  # This MUST be formatted
  afterDirectiveNested   =   30;

  # Directive in regular string interpolation
  directiveInRegularInterp = "${
/*nixfmt:disable*/
    1    +    2
/*nixfmt:enable*/
  }";

  # This MUST be formatted
  afterDirectiveRegular   =   40;

  # False positive: directive-like text in nested string content (not in code position).
  # Only disable (no enable) — if the scanner misreads this, the rest of the file breaks.
  falsePositiveNested = ''
    ${ "
      /*nixfmt:disable*/
      not a real directive
    " }
  '';

  # This MUST be formatted
  afterFalsePositive   =   50;

  # Directive inside interpolation
  # The disable has no matching enable inside the interpolation,
  # so it disables formatting for the rest of the file.
  withDirectiveInInterpolation = ''
    prefix
    ${
       1
         + 2

/*nixfmt:disable*/
  # from now on, formatting is disabled
       + 
         3
           + 
             4 /*nixfmt:enable*/
                } /*nixfmt:enable*/
                                    still disabled, because both enable directives above are invalid
                '';

  # NOT formatted (we're still in the disabled region from the interpolation above)
  stillDisabled   =   1;

  # Directive inside another interpolation to re-enable formatting
  reEnable = ''
    ${
/*nixfmt:enable*/
      1   +   2
    }
  '';

  # This MUST be formatted (proves the enable inside interpolation worked)
  afterEnable   =   3;
}
